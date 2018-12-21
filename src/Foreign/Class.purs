module Foreign.Class where

import Prelude

import Control.Monad.Except (ExceptT(..), except, mapExcept, runExceptT, withExcept)
import Data.Array ((..), zipWith, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Foreign (F, Foreign, ForeignError(..), readArray, readBoolean, readChar, readInt, readNumber, readString, unsafeToForeign)
import Foreign.Index (readProp)
import Foreign.Internal (readObject)
import Foreign.NullOrUndefined (readNullOrUndefined, undefined)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (get)
import Record.Builder (Builder)
import Record.Builder (build, insert) as Builder
import Type.Prelude (RLProxy(..))

-- | The `Decode` class is used to generate decoding functions
-- | of the form `Foreign -> F a` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericDecode` and `genericDecodeJSON` functions
-- | to decode your foreign/JSON-encoded data.
class Decode a where
  decode :: Foreign -> F a

instance voidDecode :: Decode Void where
  decode _ = except (Left (pure (ForeignError "Decode: void")))

instance unitDecode :: Decode Unit where
  decode _ = pure unit

instance foreignDecode :: Decode Foreign where
  decode = pure

instance stringDecode :: Decode String where
  decode = readString

instance charDecode :: Decode Char where
  decode = readChar

instance booleanDecode :: Decode Boolean where
  decode = readBoolean

instance numberDecode :: Decode Number where
  decode = readNumber

instance intDecode :: Decode Int where
  decode = readInt

instance arrayDecode :: Decode a => Decode (Array a) where
  decode = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (decode value)

instance maybeDecode :: Decode a => Decode (Maybe a) where
  decode = readNullOrUndefined decode

instance objectDecode :: Decode v => Decode (Object v) where
  decode = sequence <<< Object.mapWithKey (\_ -> decode) <=< readObject

instance recordDecode :: (RowToList fields fieldList, DecodeForeignFields fieldList () fields) => Decode (Record fields) where
  decode o = flip Builder.build {} <$> getFields (RLProxy :: RLProxy fieldList) o

class DecodeForeignFields (xs :: RowList) (from :: # Type) (to :: # Type) | xs -> from to where
  getFields :: RLProxy xs -> Foreign -> F (Builder (Record from) (Record to))

instance decodeFieldsCons ::
  ( IsSymbol name
  , Decode a
  , DecodeForeignFields tail from from'
  , Row.Lacks name from'
  , Row.Cons name a from' to
  ) => DecodeForeignFields (Cons name a tail) from to where
  getFields _ o = (compose <$> first) `exceptTApply` rest
    where
      first = do
        value <- withExcept' (readProp name o >>= decode)
        pure $ Builder.insert nameP value
      rest = getFields tailP o
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtProperty name

exceptTApply :: forall a b e m. Semigroup e => Applicative m => ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
exceptTApply f a = ExceptT $ applyEither <$> runExceptT f <*> runExceptT a

applyEither :: forall e a b. Semigroup e => Either e (a -> b) -> Either e a -> Either e b
applyEither (Left e) (Right _) = Left e
applyEither (Left e1) (Left e2) = Left (e1 <> e2)
applyEither (Right _) (Left e) = Left e
applyEither (Right f) (Right a) = Right (f a)

instance decodeFieldsNil :: DecodeForeignFields Nil () () where
  getFields _ _ = pure identity


-- | The `Encode` class is used to generate encoding functions
-- | of the form `a -> Foreign` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericEncode` and `genericEncodeJSON` functions
-- | to encode your data as JSON.
class Encode a where
  encode :: a -> Foreign

instance voidEncode :: Encode Void where
  encode = absurd

instance unitEncode :: Encode Unit where
  encode _ = unsafeToForeign {}

instance foreignEncode :: Encode Foreign where
  encode = identity

instance stringEncode :: Encode String where
  encode = unsafeToForeign

instance charEncode :: Encode Char where
  encode = unsafeToForeign

instance booleanEncode :: Encode Boolean where
  encode = unsafeToForeign

instance numberEncode :: Encode Number where
  encode = unsafeToForeign

instance intEncode :: Encode Int where
  encode = unsafeToForeign

instance arrayEncode :: Encode a => Encode (Array a) where
  encode = unsafeToForeign <<< map encode

instance maybeEncode :: Encode a => Encode (Maybe a) where
  encode = maybe undefined encode

instance objectEncode :: Encode v => Encode (Object v) where
  encode = unsafeToForeign <<< Object.mapWithKey (\_ -> encode)

instance recordEncode :: (RowToList fields fieldList, EncodeForeignFields fieldList fields () to) => Encode (Record fields) where
  encode r = unsafeToForeign $ Builder.build (encodeFields (RLProxy :: RLProxy fieldList) r) {}

class EncodeForeignFields (xs :: RowList) (row :: # Type) (from :: # Type) (to :: # Type) | xs -> row from to where
  encodeFields :: forall g. g xs -> Record row -> Builder (Record from) (Record to)

instance consEncodeForeignFields ::
  ( IsSymbol name
  , Encode a
  , EncodeForeignFields tail row from from'
  , Row.Cons name a whatever row
  , Row.Lacks name from'
  , Row.Cons name Foreign from' to
  ) => EncodeForeignFields (Cons name a tail) row from to where
  encodeFields _ r = Builder.insert nameP value <<< rest
    where
      nameP = SProxy :: SProxy name
      value = encode (get nameP r)
      tailP = RLProxy :: RLProxy tail
      rest = encodeFields tailP r

instance nilEncodeForeignFields :: EncodeForeignFields Nil row () () where
  encodeFields _ _ = identity
