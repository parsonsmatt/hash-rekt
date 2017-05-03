{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
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

import           Data.Dynamic        (Dynamic, Proxy (..), Typeable,
                                      fromDynamic, toDyn)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           GHC.TypeLits        (CmpSymbol, ErrorMessage (..), KnownSymbol,
                                      Symbol, TypeError, symbolVal)

import           Prelude             hiding (lookup)

-- | A 'HashRecord' is an extensible record, containing values of
-- potentially different types and indexed by strings. The
newtype HashRecord (k :: [*]) = HashRecord { getHashRecord :: HashMap String Dynamic }

-- | The 'Record' type synonym is convenient for making type aliases for
-- records. It sorts the keys and values you provide to it. You should
-- /not/ construct these types yourself, as this breaks invariant. I should
-- probably not even export the dang type constructor.
type Record xs = HashRecord (Sort xs)

-- | A helper class to convert the values contained in a 'HashRecord' into
-- strings, so the whole thing can be shown.
class ToStringMap a where
    toStringMap :: a -> HashMap String String

-- | The empty 'HashRecord' is easy. This is equivalent to an empty
-- 'HashMap'.
instance ToStringMap (HashRecord '[]) where
    toStringMap _ = mempty

-- | This constraint alias is repeated often. In order for the symbol @k@
-- and the type @v@ to be entries in a 'HashRecord', the symbol must be an
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
    , ToStringMap (HashRecord xs)
    ) => ToStringMap (HashRecord (k =: v ': xs)) where
    toStringMap rec@(HashRecord record) =
        Map.singleton (symbolVal (Proxy @k)) (show (lookup @k rec))
        <> toStringMap (HashRecord record :: HashRecord xs)

instance ToStringMap (HashRecord xs) => Show (HashRecord xs) where
    show = show . toStringMap

-- | Defining equality on 'HashRecord's starts with the base case. Any two
-- empty 'HashRecord's are equal.
instance Eq (HashRecord '[]) where
    _ == _ = True

-- | For the inductive case, we require that the types @k@ and @v@ are
-- a valid 'MapEntry', that the @v@ type is an instance of 'Eq', and that
-- we can compare the rest of the 'HashRecord' for equality. With those
-- requirements satisfied, we can grab the element of type @v@ corresponding
-- with the key @k@ from bo;h records, and compare them for equality. If these
-- are equal, then we'll recurse, comparing the rest of the maps.
instance
    ( Eq v
    , Eq (HashRecord xs)
    , MapEntry k v
    ) => Eq (HashRecord (k =: v ': xs)) where
    HashRecord rec0 == HashRecord rec1 =
        let this = ix rec0 == ix rec1
            rest = (HashRecord rec0 :: HashRecord xs) == (HashRecord rec1 :: HashRecord xs)
            ix rec = unsafeLookup @k "HashRecord.==" rec :: v
         in this && rest

-- | Construct an empty 'HashRecord'.
empty :: HashRecord '[]
empty = HashRecord mempty

-- | '=:' is a type that pairs type level 'String's (aka 'Symbol's) with
-- a value at the term level.
data (key :: Symbol) =: (a :: *)

-- | Insert a value into the 'HashRecord'.
insert
    :: forall key val keys. MapEntry key val
    => val
    -> HashRecord keys
    -> HashRecord (InsertSorted key val keys)
insert val (HashRecord record) =
     HashRecord (Map.insert (symbolVal (Proxy @key)) (toDyn val) record)

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

-- | Looks up the given key in a 'HashRecord'. Intended to be used with
-- TypeApplications.
--
-- >>> Rec.lookup @"foo" (Rec.insert @"foo" 'a' Rec.empty)
-- 'a'
lookup
    :: forall key val keys. (MapEntry key val , Lookup' key keys ~ val)
    => HashRecord keys
    -> val
lookup = unsafeLookup @key "lookup". getHashRecord

-- | Seriously, don't do this.
unsafeLookup
    :: forall k v. MapEntry k v
    => String -- ^ The string note to call 'error' with when you done goofed
    -> HashMap String Dynamic -- ^ The raw record.
    -> v -- ^ The value. This can totally error. Beware!
unsafeLookup err =
    fromMaybe (oops "value did not have the right type: ")
    . fromDynamic
    . fromMaybe (oops "key did not exist in map: ")
    . Map.lookup (symbolVal (Proxy @k))
  where
    oops :: forall a. String -> a
    oops e = error ("HashRecord.unsafeLookup: " ++ e ++ err)

delete
    :: forall key val keys.
    ( KnownSymbol key
    , Lookup key val keys ~ val
    )
    => HashRecord keys
    -> HashRecord (Remove key keys)
delete (HashRecord record) =
    HashRecord (Map.delete (symbolVal (Proxy @key)) record)

-- | The type signature is inferred. Hooray!
testMap :: HashRecord '["foo" =: Char]
testMap = insert @"foo" 'a' empty

update
    :: forall key val1 val2 keys.
    ( MapEntry key val1
    , Typeable val2
    , Lookup' key keys ~ val1
    )
    => (val1 -> val2)
    -> HashRecord keys
    -> HashRecord (UpdateAt key val2 keys)
update f (HashRecord record) =
    HashRecord (Map.update k (symbolVal (Proxy @key)) record)
  where
    k = Just
        . toDyn
        . f
        . fromMaybe (error "HashRecord.update: The type cast failed somehow.")
        . fromDynamic
