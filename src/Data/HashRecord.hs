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
Module      : Data.HashRecord
Description : An extensible record backed by a HashMap.
Copyright   : (c) Matt Parsons, 2017
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Data.HashRecord
    ( -- * Using a 'HashRecord'
      empty
    , insert
    , update
    , delete

    -- * The 'HashRecord' type
    , HashRecord
    , Record
    , type (=:)
    ) where

import           Data.HashRecord.Internal
