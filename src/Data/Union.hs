{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:       Data.OpenUnion
-- Description:  Open unions (type-indexed co-products) for extensible effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.;
--               2018 Peter Trško
-- License:      BSD3
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Open unions (type-indexed co-products, i.e. type-indexed sums) for
-- extensible effects All operations are constant-time.
module Data.Union
    (
    -- * Open Union
      Union

    -- * Open Union Operations
    , decomp
    , weaken
    , extract

    -- * Open Union Membership Constraints
    , Member(..)
    , Members
    )
  where

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

import Data.Union.Internal
    ( Member(inj, prj)
    , Union
    , decomp
    , extract
    , weaken
    )


type family Members m r :: Constraint where
    Members (t ': c) r = (Member t r, Members c r)
    Members '[] r = ()
