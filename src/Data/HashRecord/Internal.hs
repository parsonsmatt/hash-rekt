{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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
newtype HashRecord (k :: [*]) = HashRecord (HashMap String Dynamic)

-- | The 'Record' type synonym is convenient for making type aliases for
-- records. It sorts the keys and values you provide to it.
type Record xs = HashRecord (Sort xs)

instance ToStringMap (HashRecord xs) => Show (HashRecord xs) where
    show = show . toStringMap

class ToStringMap a where
    toStringMap :: a -> HashMap String String

instance ToStringMap (HashRecord '[]) where
    toStringMap _ = mempty

instance (Typeable v, KnownSymbol k, Show v, ToStringMap (HashRecord xs)) => ToStringMap (HashRecord (k =: v ': xs)) where
    toStringMap rec@(HashRecord record) =
        Map.singleton (symbolVal (Proxy @k)) (show (lookup @k rec :: v))
        <> toStringMap (HashRecord record :: HashRecord xs)


-- | Construct an empty 'HashRecord'.
empty :: HashRecord '[]
empty = HashRecord mempty

-- | '=:' is a type that pairs type level 'String's (aka 'Symbol's) with
-- a value at the term level.
data (key :: Symbol) =: (a :: *)

insert
    :: forall key val keys.
    ( Typeable val
    , KnownSymbol key
    )
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

lookup
    :: forall key val keys.
    ( Typeable val
    , KnownSymbol key
    , Lookup key val keys ~ val
    )
    => HashRecord keys
    -> val
lookup (HashRecord record) =
    case Map.lookup (symbolVal (Proxy @key)) record of
        Nothing -> error "HashRecord.lookup: key did not exist in map."
        Just dyn  ->
            case fromDynamic dyn of
                Nothing -> error "HashRecord.lookup: value did not have the right type"
                Just a -> a

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
    ( KnownSymbol key
    , Typeable val2
    , Typeable val1
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
