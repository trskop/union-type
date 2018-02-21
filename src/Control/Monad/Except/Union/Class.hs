{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitForAll #-}
-- |
-- Module:      Control.Monad.Except.Union.Class
-- Description: Type classes for monads and monad transformers that provide
--              functionality for throwing and catching error in a specified
--              union of errors.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Type classes for monads and monad transformers that provide functionality
-- for throwing and catching error in a specified 'Union' of errors.
module Control.Monad.Except.Union.Class
    ( MonadThrowError(..)
    , MonadCatchError(..)
    , MonadTransCatchError(..)
    )
  where

import Control.Monad (Monad)
import Data.Function (flip)

import Data.Union.Internal (Member)


-- | Monad with an ability to throw an error that is a member of a 'Union' of
-- errors.
class Monad m => MonadThrowError (errs :: [*]) (m :: * -> *) | m -> errs where
    throwError :: Member e errs => e -> m a

-- | Monad indexed by a list of errors in a 'Union' that can catch\/handle top
-- most error.
class MonadCatchError (t :: [*] -> * -> *) where
    {-# MINIMAL catchError | handleError #-}

    catchError
        :: MonadThrowError errs (t errs)
        => t (e ': errs) a
        -> (e -> t errs a)
        -> t errs a
    catchError = flip handleError
    {-# INLINE catchError #-}

    handleError
        :: MonadThrowError errs (t errs)
        => (e -> t errs a)
        -> t (e ': errs) a
        -> t errs a
    handleError = flip catchError
    {-# INLINE handleError #-}

-- | Monad transformer indexed by a list of errors in a 'Union' that can
-- catch\/handle top most error.
class MonadTransCatchError (t :: [*] -> (* -> *) -> * -> *) where
    {-# MINIMAL catchErrorT | handleErrorT #-}

    catchErrorT
        :: (Monad m, MonadThrowError errs (t errs m))
        => t (e ': errs) m a
        -> (e -> t errs m a)
        -> t errs m a
    catchErrorT = flip handleErrorT
    {-# INLINE catchErrorT #-}

    handleErrorT
        :: (Monad m, MonadThrowError errs (t errs m))
        => (e -> t errs m a)
        -> t (e ': errs) m a
        -> t errs m a
    handleErrorT = flip catchErrorT
    {-# INLINE handleErrorT #-}
