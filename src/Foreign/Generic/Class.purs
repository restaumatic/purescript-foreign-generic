module Foreign.Generic.Class where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (mapExcept, runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..))
import Data.List (List(..), fromFoldable, null, singleton, toUnfoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, unsafeToForeign)
import Foreign.Class (class Encode, class Decode, encode, decode)
import Foreign.Generic.Types (Options, SumEncoding(..))
import Foreign.Index (index)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class GenericDecode a where
  decodeOpts :: Options -> Foreign -> F a

class GenericEncode a where
  encodeOpts :: Options -> a -> Foreign

class GenericDecodeArgs a where
  decodeArgs :: Options -> Int -> List Foreign -> F { result :: a
                                                    , rest :: List Foreign
                                                    , next :: Int
                                                    }
  decodeSingleRecordArg :: Maybe (Options -> Foreign -> F a)

-- | Encoded constructor argument, with information about whether it was a record.
-- | This can be used to handle the special case of a single record argument.
-- | See the `unwrapSingleRecordArguments` option.
data Arg = RecArg Foreign | PlainArg Foreign

-- | Forget the record flag.
unArg :: Arg -> Foreign
unArg (RecArg x) = x
unArg (PlainArg x) = x

class GenericEncodeArgs a where
  encodeArgs :: Options -> a -> List Arg

class GenericCountArgs a where
  countArgs :: Proxy a -> Either a Int

instance genericDecodeNoConstructors :: GenericDecode NoConstructors where
  decodeOpts opts _ = fail (ForeignError "No constructors")

instance genericEncodeNoConstructors :: GenericEncode NoConstructors where
  encodeOpts opts a = encodeOpts opts a

instance genericDecodeConstructor
  :: (IsSymbol name, GenericDecodeArgs rep, GenericCountArgs rep)
  => GenericDecode (Constructor name rep) where
  decodeOpts opts f =
      if opts.unwrapSingleConstructors
        then Constructor <$> readArguments f
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName, constructorTagTransform } -> do
                 tag <- mapExcept (lmap (map (ErrorAtProperty tagFieldName))) do
                   tag <- index f tagFieldName >>= readString
                   let expected = constructorTagTransform ctorName
                   unless (tag == expected) $
                     fail (ForeignError ("Expected " <> show expected <> " tag"))
                   pure tag
                 args <-
                   case decodeSingleRecordArg of
                     Just decodeArg | opts.unwrapSingleRecordArguments ->
                       decodeArg opts f
                     _ ->
                       mapExcept (lmap (map (ErrorAtProperty contentsFieldName)))
                         (index f contentsFieldName >>= readArguments)
                 pure (Constructor args)
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      numArgs = countArgs (Proxy :: Proxy rep)

      readArguments args =
        case numArgs of
          Left a -> pure a
          Right 1 | opts.unwrapSingleArguments -> do
            { result, rest } <- decodeArgs opts 0 (singleton args)
            unless (null rest) $
              fail (ForeignError "Expected a single argument")
            pure result
          Right n -> do
            vals <- readArray args
            { result, rest } <- decodeArgs opts 0 (fromFoldable vals)
            unless (null rest) $
              fail (ForeignError ("Expected " <> show n <> " constructor arguments"))
            pure result

-- | A way to encode different configuration of constructor arguments.
type EncodeArgs r =
  { noArgs :: r
  , singleRecord :: Object Foreign -> r
  , default :: Foreign -> r
  }

instance genericEncodeConstructor
  :: (IsSymbol name, GenericEncodeArgs rep)
  => GenericEncode (Constructor name rep) where
  encodeOpts opts (Constructor args) =
      if opts.unwrapSingleConstructors
        then encodeArgsArray
              { noArgs: unsafeToForeign {}
              , singleRecord: unsafeToForeign
              , default: identity
              } args
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName, constructorTagTransform } ->
                 let
                   encodedTag = Object.singleton tagFieldName (unsafeToForeign $ constructorTagTransform ctorName)
                   encodedArgs = encodeArgsArray
                                   { noArgs: Object.empty
                                   , singleRecord: \r ->
                                       if opts.unwrapSingleRecordArguments
                                         then r
                                         else Object.singleton contentsFieldName (unsafeToForeign r)
                                   , default: Object.singleton contentsFieldName
                                   } args
                 in
                   unsafeToForeign (encodedTag `Object.union` encodedArgs)
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      encodeArgsArray :: forall a. EncodeArgs a -> rep -> a
      encodeArgsArray encoding =
        unwrapArguments encoding <<< toUnfoldable <<< encodeArgs opts

      unwrapArguments :: forall a. EncodeArgs a -> Array Arg -> a
      unwrapArguments encoding []
        = encoding.noArgs
      unwrapArguments encoding [RecArg x]
        | Right contents <- runExcept (decode x)
          = encoding.singleRecord contents
      unwrapArguments encoding [x]
        | opts.unwrapSingleArguments
          = encoding.default (unArg x)
      unwrapArguments encoding xs
        = encoding.default (unsafeToForeign (map unArg xs))

instance genericDecodeSum
  :: (GenericDecode a, GenericDecode b)
  => GenericDecode (Sum a b) where
  decodeOpts opts f = Inl <$> decodeOpts opts' f <|> Inr <$> decodeOpts opts' f
    where
      -- Reuse the unwrapSingleConstructors flag, since we cannot have a single
      -- constructor at this point anyway.
      opts' = opts { unwrapSingleConstructors = false }

instance genericEncodeSum
  :: (GenericEncode a, GenericEncode b)
  => GenericEncode (Sum a b) where
  encodeOpts opts (Inl a) = encodeOpts (opts { unwrapSingleConstructors = false }) a
  encodeOpts opts (Inr b) = encodeOpts (opts { unwrapSingleConstructors = false }) b

instance genericDecodeArgsNoArguments :: GenericDecodeArgs NoArguments where
  decodeArgs _ i Nil = pure { result: NoArguments, rest: Nil, next: i }
  decodeArgs _ _ _ = fail (ForeignError "Too many constructor arguments")
  decodeSingleRecordArg = Nothing

instance genericEncodeArgsNoArguments :: GenericEncodeArgs NoArguments where
  encodeArgs _ = mempty

instance genericDecodeArgsRecord
  :: Decode_ (Record a)
  => GenericDecodeArgs (Argument (Record a)) where
  decodeArgs = decodeArgsArgument
  decodeSingleRecordArg = Just (\options i -> Argument <$> decode_ options i)
else instance genericDecodeArgsArgument
  :: Decode_ a
  => GenericDecodeArgs (Argument a) where
  decodeArgs = decodeArgsArgument
  decodeSingleRecordArg = Nothing

decodeArgsArgument
  :: forall a
   . Decode_ a
  => Options -> Int -> List Foreign
  -> F { result :: Argument a, rest :: List Foreign, next :: Int }
decodeArgsArgument opts i (x : xs) = do
  a <- mapExcept (lmap (map (ErrorAtIndex i))) (decode_ opts x)
  pure { result: Argument a, rest: xs, next: i + 1 }
decodeArgsArgument _ _ _ = fail (ForeignError "Not enough constructor arguments")

instance genericEncodeArgsRecord
  :: Encode_ (Record a)
  => GenericEncodeArgs (Argument (Record a)) where
  encodeArgs opts (Argument a) = singleton (RecArg (encode_ opts a))
else instance genericEncodeArgsArgument
  :: Encode_ a
  => GenericEncodeArgs (Argument a) where
  encodeArgs opts (Argument a) = singleton (PlainArg (encode_ opts a))

instance genericDecodeArgsProduct
  :: (GenericDecodeArgs a, GenericDecodeArgs b)
  => GenericDecodeArgs (Product a b) where
  decodeArgs opts i xs = do
    { result: resA, rest: xs1, next: i1 } <- decodeArgs opts i xs
    { result: resB, rest, next } <- decodeArgs opts i1 xs1
    pure { result: Product resA resB, rest, next }
  decodeSingleRecordArg = Nothing

instance genericEncodeArgsProduct
  :: (GenericEncodeArgs a, GenericEncodeArgs b)
  => GenericEncodeArgs (Product a b) where
  encodeArgs opts (Product a b) = encodeArgs opts a <> encodeArgs opts b

instance genericCountArgsNoArguments :: GenericCountArgs NoArguments where
  countArgs _ = Left NoArguments

instance genericCountArgsArgument :: GenericCountArgs (Argument a) where
  countArgs _ = Right 1

instance genericCountArgsProduct
  :: (GenericCountArgs a, GenericCountArgs b)
  => GenericCountArgs (Product a b) where
  countArgs _ =
    case countArgs (Proxy :: Proxy a), countArgs (Proxy :: Proxy b) of
      Left a , Left b  -> Left (Product a b)
      Left _ , Right n -> Right n
      Right n, Left _  -> Right n
      Right n, Right m -> Right (n + m)


class Decode_ a where
  decode_ :: Options -> Foreign -> F a

class Encode_ a where
  encode_ :: Options -> a -> Foreign

instance decode_Record :: (RowToList r rl, DecodeRecord r rl) => Decode_ (Record r) where
  decode_ opts = map (flip Builder.build {}) <$> decodeRecord_ (RLProxy :: RLProxy rl) opts
else instance decode_Other :: Decode a => Decode_ a where
  decode_ _ = decode

instance encode_Record :: (RowToList r rl, EncodeRecord r rl) => Encode_ (Record r) where
  encode_ opts = unsafeToForeign <<< encodeRecord_ (RLProxy :: RLProxy rl) opts
else instance encode_Other :: Encode a => Encode_ a where
  encode_ _ = encode
  
class DecodeRecord r rl | rl -> r where
  decodeRecord_ :: RLProxy rl -> Options -> Foreign -> F (Builder {} (Record r))

class EncodeRecord r rl | rl -> r where
  encodeRecord_ :: RLProxy rl -> Options -> Record r -> Object Foreign

instance decodeRecordNil :: DecodeRecord () Nil where
  decodeRecord_ _ _ _ = pure identity

instance encodeRecordNil :: EncodeRecord () Nil where
  encodeRecord_ _ _ _ = Object.empty

instance decodeRecordCons 
    :: ( Cons l a r_ r
       , DecodeRecord r_ rl_
       , IsSymbol l
       , Decode_ a
       , Lacks l r_
       )
    => DecodeRecord r (Cons l a rl_)
  where
    decodeRecord_ _ opts f = do
      builder <- decodeRecord_ (RLProxy :: RLProxy rl_) opts f
      let l = reflectSymbol (SProxy :: SProxy l)
          l_transformed = (opts.fieldTransform l)
      f_ <- index f l_transformed
      a <- mapExcept (lmap (map (ErrorAtProperty l_transformed))) (decode_ opts f_)
      pure (builder >>> Builder.insert (SProxy :: SProxy l) a)

instance encodeRecordCons 
    :: ( Cons l a r_ r
       , EncodeRecord r_ rl_
       , IsSymbol l
       , Encode_ a
       )
    => EncodeRecord r (Cons l a rl_) 
  where
    encodeRecord_ _ opts rec = 
      let obj = encodeRecord_ (RLProxy :: RLProxy rl_) opts (unsafeCoerce rec)
          l = reflectSymbol (SProxy :: SProxy l)
       in Object.insert (opts.fieldTransform l) (encode_ opts (Record.get (SProxy :: SProxy l) rec)) obj
    
