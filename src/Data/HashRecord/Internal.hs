{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Data.HashRecord.Internal
Description : An extensible record backed by a HashMap.
Copyright   : (c) Matt Parsons, 2017
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This is an internal module. Depend upon it at your own risk -- breaking
changes in here will /not/ be reflected in the major API version.

-}
module Data.HashRecord.Internal where

import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Dynamic         (Dynamic, Proxy (..), Typeable,
                                       fromDynamic, toDyn)
import           Data.Functor.Classes
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import qualified Data.Text            as Text
import           GHC.TypeLits         (CmpSymbol, ErrorMessage (..),
                                       KnownSymbol, Symbol, TypeError,
                                       symbolVal)

import           Prelude              hiding (lookup)

-- | A 'HashRecord' is an extensible record, containing values of
-- potentially different types and indexed by strings. The values are
-- parameterized over some type constructor @f@, which will often be 'Identity'
-- for ordinary records, or 'Maybe' for partial records, or 'Either' for records
-- under validation.
--
-- The functions and types that operate on 'Identity' based ones will be bare,
-- while functions operating on the more general 'HashRecord'' type will have
-- a @'@ suffix.
newtype HashRecord' f (k :: [*])
    = HashRecord
    { getHashRecord :: HashMap String (f Dynamic)
    }

type HashRecord = HashRecord' Identity

-- | The 'Record' type synonym is convenient for making type aliases for
-- records. It sorts the keys and values you provide to it. You should
-- /not/ construct these types yourself, as this breaks invariant. I should
-- probably not even export the dang type constructor.
type Record' f xs = HashRecord' f (Sort xs)

type Record xs = Record' Identity xs

-- | A helper class to convert the values contained in a 'HashRecord' into
-- strings, so the whole thing can be shown.
class ToStringMap a where
    toStringMap :: a -> HashMap String String

-- | The empty 'HashRecord' is easy. This is equivalent to an empty
-- 'HashMap'.
instance Show1 f => ToStringMap (HashRecord' f '[]) where
    toStringMap _ = mempty

-- | This constraint alias is repeated often. In order for the symbol @k@
-- and the type @v@ to be entries in a 'HashRecord'', the symbol must be an
-- instance of 'KnownSymbol' and the type must be an instance of
-- 'Typeable'.
type MapEntry k v = (Typeable v, KnownSymbol k)

-- | The inductive case is a little trickier. We pluck the first value out
-- of the map. This requires that the @k@ symbol is a 'KnownSymbol', so
-- that we can reflect the value and get the 'String' back. We index into
-- the current record with that string, which requires the 'Typeable'
-- constraint on the value. Finally,
instance
    ( MapEntry k v
    , Show v
    , Show1 f
    , Functor f
    , ToStringMap (HashRecord' f xs)
    ) => ToStringMap (HashRecord' f (k =: v ': xs)) where
    toStringMap rec@(HashRecord record) =
        Map.singleton (symbolVal (Proxy @k)) (showsPrec1 0 (lookup' @k rec) "")
        <> toStringMap (HashRecord record :: HashRecord' f xs)

instance ToStringMap (HashRecord' f xs) => Show (HashRecord' f xs) where
    show = show . toStringMap

-- | Defining equality on 'HashRecord''s starts with the base case. Any two
-- empty 'HashRecord''s are equal.
instance Eq1 f => Eq (HashRecord' f '[]) where
    _ == _ = True

-- | For the inductive case, we require that the types @k@ and @v@ are
-- a valid 'MapEntry', that the @v@ type is an instance of 'Eq', and that
-- we can compare the rest of the 'HashRecord'' for equality. With those
-- requirements satisfied, we can grab the element of type @v@ corresponding
-- with the key @k@ from bo;h records, and compare them for equality. If these
-- are equal, then we'll recurse, comparing the rest of the maps.
instance
    ( Eq v
    , Eq1 f
    , Eq (HashRecord' f xs)
    , Functor f
    , MapEntry k v
    ) => Eq (HashRecord' f (k =: v ': xs)) where
    HashRecord rec0 == HashRecord rec1 =
        let this = ix' rec0 `eq1` ix' rec1
            rest = (HashRecord rec0 :: HashRecord' f xs) == (HashRecord rec1 :: HashRecord' f xs)
            ix' rec = unsafeLookup @k "HashRecord'.==" rec :: f v
         in this && rest

instance ToPairs (HashRecord' f xs) => ToJSON (HashRecord' f xs) where
    toJSON = object . toPairs

-- | An auxiliary helper class to make it easier to define the 'ToJSON' class
-- for 'HashRecord''.
class ToPairs a where
    toPairs :: a -> [Pair]

-- | This is another example of inductive type class definitions. The base case
-- is the empty map, which is just the empty list.
instance ToJSON1 f => ToPairs (HashRecord' f '[]) where
    toPairs _ = []

-- | To define the inductive step, we require that that the @k@ symbol is
-- a 'KnownSymbol' to GHC, that we can convert the @v@ value 'ToJSON', and that
-- the rest of the 'HashRecord'' can be converted 'ToPairs'.
instance
    ( MapEntry k v
    , ToJSON v
    , ToJSON1 f
    , Functor f
    , ToPairs (HashRecord' f xs)
    ) => ToPairs (HashRecord' f (k =: v ': xs)) where
    toPairs rec@(HashRecord record) =
        let this = Text.pack (symbolVal (Proxy @k)) .= toJSON1 (lookup' @k rec)
            rest = toPairs (HashRecord record :: HashRecord' f xs)
         in this : rest

instance FromJSON (HashRecord' f '[]) where
    parseJSON = withObject "HashRecord'" $ \_ -> pure (HashRecord mempty)

instance
    ( FromJSON1 f
    , MapEntry k v
    , FromJSON v
    , FromJSON (HashRecord' f xs)
    , Applicative f
    ) => FromJSON (HashRecord' f (k =: v ': xs)) where
    parseJSON = withObject "HashRecord'" $ \o -> do
        let key = symbolVal (Proxy @k)
        val :: v <- o .: Text.pack key
        HashRecord rest :: HashRecord' f xs <- parseJSON (Object o)
        pure (HashRecord (Map.insert key (pure (toDyn val)) rest))

-- | Construct an empty 'HashRecord'.
empty :: HashRecord '[]
empty = empty

-- | Construct an empty 'HashRecord''.
empty' :: HashRecord' f '[]
empty' = HashRecord mempty

-- | Insert a value into the 'HashRecord''.
--
-- >>> insert @"foo" 'a' empty
-- fromList [("foo", "'a'")]
insert'
    :: forall key val keys f. (MapEntry key val, Functor f)
    => f val
    -> HashRecord' f keys
    -> HashRecord' f (InsertSorted key val keys)
insert' val = HashRecord
    . Map.insert (symbolVal (Proxy @key)) (toDyn <$> val)
    . getHashRecord

-- | Insert a value into the 'HashRecord''.
--
-- >>> insert @"foo" 'a' empty
-- fromList [("foo", "'a'")]
insert
    :: forall key val keys f. (MapEntry key val, Applicative f)
    => val
    -> HashRecord' f keys
    -> HashRecord' f (InsertSorted key val keys)
insert val = insert' @key (pure val)

-- | Construct a singleton 'HashRecord'.
singleton'
    :: forall key val f. (Applicative f, MapEntry key val)
    => f val -> HashRecord' f '[key =: val]
singleton' v = insert' @key v empty'

-- | Construct a singleton 'HashRecord'.
singleton
    :: forall key val f. (Applicative f, MapEntry key val)
    => val -> HashRecord' f '[key =: val]
singleton v = insert @key v empty'

-- | '=:' is a type that pairs a type level 'String' (aka 'Symbol's) with
-- an inhabited type.
data (key :: Symbol) =: (a :: *)

-- | This type family looks up the key in the given list of key value
-- pairs, failing if the value is not what was anticipated or if the value
-- is not found in the map.
type family Lookup key val key'vals where
    Lookup k v '[] = TypeError (
        'Text "The key \"" ':<>: 'Text k ':<>: 'Text "\" did not exist in the map."
        ':$$: 'Text "Therefore, we can't get the value out of it."
        )
    Lookup k a (k =: a ': xs) = a
    Lookup k b (k =: a ': xs) = TypeError (
        'Text "The key \"" ':<>: 'Text k ':<>: 'Text "\" existed in the map, but contained the wrong type of value."
        ':$$: 'Text "Expected type: " ':<>: 'ShowType b
        ':$$: 'Text "But the map contained: " ':<>: 'ShowType a
        )
    Lookup k a (x =: b ': xs) = Lookup k a xs

-- | This type family looks up the key in the list of key value pairs. If
-- it doesn't exist, then it fails with a type error. Otherwise, it returns
-- the key.
type family Lookup' key key'vals where
    Lookup' k '[] = TypeError (
        'Text "The key \"" ':<>: 'Text k ':<>: 'Text "\" did not exist in the map."
        ':$$: 'Text "Therefore, we can't get the value out of it."
        )
    Lookup' k (k =: a ': xs) = a
    Lookup' k (x =: b ': xs) = Lookup' k xs

type family Remove key keys where
    Remove k '[] = '[]
    Remove k (k =: a ': xs) = xs
    Remove k (x =: b ': xs) = Remove k xs

type family InsertSorted key val keys where
    InsertSorted key val '[] =
        '[key =: val]
    InsertSorted key1 a (key2 =: b ': xs) =
        InsertSortedHelper key1 a key2 b xs (CmpSymbol key1 key2)

type family InsertSortedHelper key1 val1 key2 val2 keys cmp where
    InsertSortedHelper k1 v1 k2 v2 xs 'EQ =
        k1 =: v1 ': xs
    InsertSortedHelper k1 v1 k2 v2 xs 'LT =
        k1 =: v1 ': k2 =: v2 ': xs
    InsertSortedHelper k1 v1 k2 v2 xs 'GT =
        k2 =: v2 ': InsertSorted k1 v1 xs

type family UpdateAt key val keys where
    UpdateAt k v '[] = TypeError (
        'Text "The key \"" ':<>: 'Text k ':<>: 'Text "\" does not appear in the map."
        ':$$: 'Text "Therefore, we can't update the type of it."
        )
    UpdateAt k v (k =: x ': xs) = k =: v ': xs
    UpdateAt k v (x =: y ': xs) = x =: y ': UpdateAt k v xs

type Sort xs = SortHelper '[] xs

type family SortHelper xs ys where
    SortHelper acc '[] = acc
    SortHelper acc (k =: v ': xs) = SortHelper (InsertSorted k v acc) xs

type Union xs ys = UnionHelper '[] xs ys

type family UnionHelper acc xs ys where
    UnionHelper acc '[] '[] = acc
    UnionHelper acc (k =: v ': xs) ys = UnionHelper (InsertSorted k v acc) xs ys
    UnionHelper acc '[] (k =: v ': xs) = UnionHelper (InsertSorted k v acc) '[] xs

-- | Looks up the given key in a 'HashRecord''. Intended to be used with
-- TypeApplications.
--
-- >>> Rec.lookup @"foo" (Rec.insert @"foo" 'a' Rec.empty)
-- 'a'
lookup'
    :: forall key val keys f.
    ( MapEntry key val
    , Lookup' key keys ~ val
    , Functor f
    )
    => HashRecord' f keys
    -> f val
lookup' = unsafeLookup @key "lookup" . getHashRecord

lookup
    :: forall key val keys.
    ( MapEntry key val
    , Lookup' key keys ~ val
    )
    => HashRecord keys
    -> val
lookup = runIdentity . lookup' @key

-- | Seriously, don't do this.
unsafeLookup
    :: forall k v f. (MapEntry k v, Functor f)
    => String -- ^ The string note to call 'error' with when you done goofed
    -> HashMap String (f Dynamic) -- ^ The raw record.
    -> f v -- ^ The value. This can totally error. Beware!
unsafeLookup err =
    fmap
        (fromMaybe (oops "value did not have the right type: ")
        . fromDynamic
        )
    . fromMaybe (oops "key did not exist in map: ")
    . Map.lookup (symbolVal (Proxy @k))
  where
    oops :: forall a. String -> a
    oops e = error ("HashRecord'.unsafeLookup: " ++ e ++ err)

delete
    :: forall key val keys f.
    ( KnownSymbol key
    , Lookup key val keys ~ val
    )
    => HashRecord' f keys
    -> HashRecord' f (Remove key keys)
delete = HashRecord
    . Map.delete (symbolVal (Proxy @key))
    . getHashRecord

-- | The type signature is inferred. Hooray!
testMap :: HashRecord '["foo" =: Char]
testMap = insert @"foo" 'a' empty

-- | Take the union of the two maps. This function is left biased,
union :: HashRecord' f keys1 -> HashRecord' f keys2 -> HashRecord' f (Union keys1 keys2)
union (HashRecord r0) (HashRecord r1) = HashRecord (Map.union r0 r1)

-- | 'field' provides a lens into a 'HashRecord'', which lets you use all the fun
-- lens functions like 'view', 'over', 'set', etc.
--
-- This lens requires that the field exists in the map, so you can't use it to
-- insert new values into the map. This is deeply unfortunate, but it prevents
-- you from trying to 'view' a field that doesn't exist. I bet there's
-- a 'Traversal' or some similar business that can allow setting of fields.
--
-- >>> insert @"foo" 'a' empty ^. field @"foo"
-- 'a'
field
    :: forall key val1 val2 keys keys'.
    ( MapEntry key val1
    , Typeable val2
    , Lookup' key keys ~ val1
    , UpdateAt key val2 keys ~ keys'
    )
    => Lens (HashRecord keys) (HashRecord keys') val1 val2
field = lens getter setter
  where
    getter :: HashRecord keys -> val1
    getter = lookup @key
    setter :: HashRecord keys -> val2 -> HashRecord keys'
    setter rec val2 = update @key (const val2) rec

-- | This updates the value stored in the 'HashRecord'', potentially
-- changing it's type. Unlike 'Data.HashMap.Strict.update', this function
-- does not allow you to remove the entry if present. To remove an entry,
-- you'll need to use 'delete'.
--
-- >>> lookup @"foo" (update @"foo" succ (insert @"foo" 'a' empty))
-- 'b'
update
    :: forall key val1 val2 keys.
    ( MapEntry key val1
    , Typeable val2
    , Lookup' key keys ~ val1
    )
    => (val1 -> val2)
    -> HashRecord keys
    -> HashRecord (UpdateAt key val2 keys)
update f = HashRecord
    . Map.update k (symbolVal (Proxy @key))
    . getHashRecord
  where
    k = Just
        . Identity
        . toDyn
        . f
        . fromMaybe (error "HashRecord'.update: The type cast failed somehow.")
        . fromDynamic
        . runIdentity
