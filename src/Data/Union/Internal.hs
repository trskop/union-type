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

-- GHC >=7.10 deprecated OverlappingInstances in favour of instance by instance
-- annotation using OVERLAPPABLE and OVERLAPPING pragmas.
#ifdef DEPRECATED_LANGUAGE_OVERLAPPING_INSTANCES
#define PRAGMA_OVERLAPPABLE {-# OVERLAPPABLE #-}
#else
{-# LANGUAGE OverlappingInstances #-}
#define PRAGMA_OVERLAPPABLE
#endif

-- |
-- Module:       Data.Union.Internal
-- Description:  Open unions (type-indexed co-products) for extensible effects.
--
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.;
--               2018 Peter Trško
-- License:      BSD3
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- These are internal definitions and should be used with caution. There are no
-- guarantees that the API of this module will be preserved between minor
-- versions of this package.
--
-- Open unions (type-indexed co-products, i.e. type-indexed sums) for
-- extensible effects All operations are constant-time.
--
-- Based on
-- <http://okmij.org/ftp/Haskell/extensible/OpenUnion51.hs OpenUnion51.hs>.
--
-- Type-list @r :: [*]@ of open union components is a small Universe.
-- Therefore, we can use a @Typeable@-like evidence in that universe. In our
-- case a simple index of an element in the type-list is sufficient
-- substitution for @Typeable@.
module Data.Union.Internal (module Data.Union.Internal)
  where

import Prelude ((+), (-))

import Data.Bool (Bool, otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Word (Word)
import Unsafe.Coerce (unsafeCoerce)


-- | Open union is a strong sum (existential with an evidence).
data Union (r :: [*]) where
    Union :: {-# UNPACK #-} !Word -> t -> Union r

-- | Takes a request of type @t :: * -> *@, and injects it into the 'Union'.
--
-- Summand is assigning a specified 'Word' value, which is a position in the
-- type-list @(t ': r) :: * -> *@.
--
-- __This function is unsafe.__
--
-- /O(1)/
unsafeInj :: Word -> t -> Union r
unsafeInj = Union
{-# INLINE unsafeInj #-}

-- | Project a value of type @'Union' (t ': r) :: * -> *@ into a possible
-- summand of the type @t :: * -> *@. 'Nothing' means that @t :: * -> *@ is not
-- the value stored in the @'Union' (t ': r) :: * -> *@.
--
-- It is assumed that summand is stored in the 'Union' when the 'Word' value is
-- the same value as is stored in the 'Union'.
--
-- __This function is unsafe.__
--
-- /O(1)/
unsafePrj :: Word -> Union r -> Maybe t
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}

-- | Represents position of element @t :: * -> *@ in a type list
-- @r :: [* -> *]@.
newtype P t r = P {unP :: Word}

-- | Find an index of an element @t :: * -> *@ in a type list @r :: [* -> *]@.
-- The element must exist.
--
-- This is essentially a compile-time computation without run-time overhead.
class FindElem (t :: *) (r :: [*]) where
    -- | Position of the element @t :: * -> *@ in a type list @r :: [* -> *]@.
    --
    -- Position is computed during compilation, i.e. there is no run-time
    -- overhead.
    --
    -- /O(1)/
    elemNo :: P t r

-- | Base case; element is at the current position in the list.
instance FindElem t (t ': r) where
    elemNo = P 0

-- | Recursion; element is not at the current position, but is somewhere in the
-- list.
instance PRAGMA_OVERLAPPABLE FindElem t r => FindElem t (t' ': r) where
    elemNo = P $ 1 + unP (elemNo :: P t r)

-- | This type class is used for two following purposes:
--
-- * As a @Constraint@ it guarantees that @t :: * -> *@ is a member of a
--   type-list @r :: [* -> *]@.
--
-- * Provides a way how to inject\/project @t :: * -> *@ into\/from a 'Union',
--   respectively.
--
-- Following law has to hold:
--
-- @
-- 'prj' . 'inj' === 'Just'
-- @
class FindElem t r => Member (t :: *) r where
    -- | Takes a request of type @t :: * -> *@, and injects it into the
    -- 'Union'.
    --
    -- /O(1)/
    inj :: t -> Union r

    -- | Project a value of type @'Union' (t ': r) :: * -> *@ into a possible
    -- summand of the type @t :: * -> *@. 'Nothing' means that @t :: * -> *@ is
    -- not the value stored in the @'Union' (t ': r) :: * -> *@.
    --
    -- /O(1)/
    prj :: Union r -> Maybe t

instance FindElem t r => Member t r where
    inj = unsafeInj $ unP (elemNo :: P t r)
    {-# INLINE inj #-}

    prj = unsafePrj $ unP (elemNo :: P t r)
    {-# INLINE prj #-}

-- | Orthogonal decomposition of a @'Union' (t ': r) :: * -> *@. 'Right' value
-- is returned if the @'Union' (t ': r) :: * -> *@ contains @t :: * -> *@, and
-- 'Left' when it doesn't. Notice that 'Left' value contains
-- @Union r :: * -> *@, i.e. it can not contain @t :: * -> *@.
--
-- /O(1)/
decomp :: Union (t ': r) -> Either (Union r) t
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}

-- | Specialized version of 'decomp' for efficiency.
--
-- /O(1)/
--
-- TODO: Check that it actually adds on efficiency.
decomp0 :: Union '[t] -> Either (Union '[]) t
decomp0 (Union _ a) = Right $ unsafeCoerce a
{-# INLINE decomp0 #-}
{-# RULES "decomp/singleton"  decomp = decomp0 #-}

-- | Specialised version of 'prj'\/'decomp' that works on an
-- @'Union' '[t] :: * -> *@ which contains only one specific summand. Hence the
-- absence of 'Maybe', and 'Either'.
--
-- /O(1)/
extract :: Union '[t] -> t
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}

-- | Inject whole @'Union' r@ into a weaker @'Union' (any ': r)@ that has one
-- more summand.
--
-- /O(1)/
weaken :: Union r -> Union (any ': r)
weaken (Union n a) = Union (n + 1) a
{-# INLINE weaken #-}

-- | Compare two open unions if they contain value of the same type, but it
-- makes no guarantees about those values being equal.
--
-- /O(1)/
eqMember :: r ~ r' => Union r -> Union r' -> Bool
eqMember (Union n1 _) (Union n2 _) = n1 == n2

-- | Check if open contains specific value.
--
-- /O(1)/
is :: (Member t r, Eq t) => t -> Union r -> Bool
is t u = Just t == prj u
