{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:      Control.Monad.Except.Union
-- Description: Variant of ExceptT with open union of errors.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Variant of ExceptT with open union of errors.
module Control.Monad.Except.Union
    (
    -- * The UnionExceptT monad transformer
      UnionExceptT(..)
    , unionExceptT
    , runUnionExceptT

    , mapUnionExceptT
    , withUnionExceptT

    , weakenE
    , weakenE2

    -- * Exception operations
    , throwE
    , catchE
    , handleE
    )
  where

import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (first)
import Data.Coerce (coerce)

import Control.Monad.Except (ExceptT(ExceptT), MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans)

import Data.Union.Internal


-- {{{ UnionExceptT -----------------------------------------------------------

newtype UnionExceptT errs m a = UnionExceptT
    { runUnionExceptT' :: ExceptT (Union errs) m a
    -- ^ Consider using 'runUnionExceptT' instead.
    }
  deriving
    ( Applicative
    , Foldable
    , Functor
    , Monad
    , MonadError (Union errs)
    , MonadFail
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

-- | Smart constructor for 'UnionExceptT'.
unionExceptT :: m (Either (Union errs) a) -> UnionExceptT errs m a
unionExceptT = coerce
{-# INLINE unionExceptT #-}

withUnionExceptT
    :: Functor m
    => (Union errs -> Union errs')
    -> UnionExceptT errs m a
    -> UnionExceptT errs' m a
withUnionExceptT f = mapUnionExceptT (fmap (first f))
{-# INLINE withUnionExceptT #-}

mapUnionExceptT
    :: (m (Either (Union errs) a) -> n (Either (Union errs') b))
    -> UnionExceptT errs m a
    -> UnionExceptT errs' n b
mapUnionExceptT = coerce
{-# INLINE mapUnionExceptT #-}

weakenE :: Functor m => UnionExceptT errs m a -> UnionExceptT (e ': errs) m a
weakenE = mapUnionExceptT (first weaken <$>)
{-# INLINE weakenE #-}

weakenE2
    :: Functor m
    => UnionExceptT errs m a
    -> UnionExceptT (e1 ': e2 ': errs) m a
weakenE2 = mapUnionExceptT (first (weaken . weaken) <$>)

-- }}} UnionExceptT -----------------------------------------------------------

-- {{{ Exception operations ---------------------------------------------------

{-# INLINE weakenE2 #-}
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

-- {{{ Exception operations ---------------------------------------------------
