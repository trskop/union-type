{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:      Control.Monad.Except.Union
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Control.Monad.Except.Union
--  (
--  )
  where

import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (first)
import Data.Coerce (coerce)

import Control.Monad.Except
    ( ExceptT(ExceptT)
    , MonadError(catchError, throwError)
    )
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans)

import Data.Union.Internal


newtype UnionExceptT errs m a = UnionExceptT
    { _runUnionExceptT :: ExceptT (Union errs) m a
    }
  deriving
    ( Applicative
    , Foldable
    , Functor
    , Monad
    , MonadError (Union errs)
    , MonadIO
    , MonadRWS r w s
    , MonadReader r
    , MonadState s
    , MonadTrans
    , MonadWriter w
    )

runUnionExceptT :: UnionExceptT errs m a -> m (Either (Union errs) a)
runUnionExceptT = coerce
{-# INLINE runUnionExceptT #-}

unionExceptT :: m (Either (Union errs) a) -> UnionExceptT errs m a
unionExceptT = coerce
{-# INLINE unionExceptT #-}

mapUnionExceptT
    :: (m (Either (Union errs) a) -> n (Either (Union errs') b))
    -> UnionExceptT errs m a
    -> UnionExceptT errs' n b
mapUnionExceptT = coerce
{-# INLINE mapUnionExceptT #-}

throwE
    :: (Monad m, Member e errs)
    => e
    -> UnionExceptT errs m a
throwE = unionExceptT . pure . Left . inj
{-# INLINE throwE #-}

handleE
    :: Monad m
    => (e -> UnionExceptT errs m a)
    -> UnionExceptT (e ': errs) m a
    -> UnionExceptT errs m a
handleE f = mapUnionExceptT (>>= either handleE' (pure . Right))
  where
    handleE' = either (pure . Left) (runUnionExceptT . f) . decomp
{-# INLINE handleE #-}

catchE
    :: Monad m
    => UnionExceptT (e ': errs) m a
    -> (e -> UnionExceptT errs m a)
    -> UnionExceptT errs m a
catchE = flip handleE
{-# INLINE catchE #-}

weakenE :: Functor m => UnionExceptT errs m a -> UnionExceptT (e ': errs) m a
weakenE = mapUnionExceptT (first weaken <$>)
{-# INLINE weakenE #-}

weakenE2
    :: Functor m
    => UnionExceptT errs m a
    -> UnionExceptT (e1 ': e2 ': errs) m a
weakenE2 = mapUnionExceptT (first (weaken . weaken) <$>)
{-# INLINE weakenE2 #-}
