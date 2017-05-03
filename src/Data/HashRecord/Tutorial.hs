{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Data.HashRecord.Tutorial where

import           Data.HashRecord (type (=:), Record)
import qualified Data.HashRecord as Rec

-- * Haskell Records

-- $records
--
-- Haskell records are often considered a pain point in the language. Beginners
-- to Haskell declare records like:
--
-- @
-- data User = User { id :: Int, name :: String }
--
-- data Blog = Blog { id :: Int, user :: User }
-- @
--
-- and are frustrated to find that the conflicting field name @id@ can't be
-- reused for the modules. This has a good reason: Haskell record definitions
-- translate to something like:
--
-- @
-- data User = User Int String
--
-- id :: User -> Int
-- id (User i _) = i
--
-- name :: User -> String
-- name (User _ n) = n
-- @
--
-- When the language attempts to generate the @id@ function for 'Blog', it runs
-- into an issue: we've already defined @id :: User -> Int@! Haskell doesn't
-- allow ordinary functions to be overloaded.
--
-- The *best* solution to something like this is to use the awesome
-- "Control.Lens" library, and the function 'Control.Lens.makeFields'. This
-- permits you to write:
--
-- @
-- data User = User { userId :: Int, userName :: String }
--
-- data Blog = Blog { blogId :: Int, blogUser :: User }
--
-- makeFields ''User
-- makeFields ''Blog
-- @
--
-- And it will generate polymorphic lenses @id@, @name@, and @user@ which can be
-- used on *any* data type that has those fields. This solves the vast majority
-- of record problems, and I'd recommend sticking with that for most cases. The
-- other solutions get rather complex, and you're better off reaching for the
-- least power that you need.

-- * The Other Problems

-- $otherproblems
--
-- There's another set of problems with records, though. Sometimes, you have two
-- types that are super similar, and you want to factor that out.

type User = Record ["id" =: Int, "name" =: String]

exampleUser1 :: User
exampleUser1 = Rec.insert @"id" 0
    . Rec.insert @"name" "My Name"
    $ Rec.empty


