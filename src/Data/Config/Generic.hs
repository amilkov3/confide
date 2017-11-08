{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Config.Generic( FromConf(..) ) where

import qualified Data.Text as T
import GHC.Generics
import Data.Config
import Data.Proxy
import System.IO.Unsafe
import Control.Monad.Catch (MonadThrow)

class FromConf a where
  get :: MonadThrow m => T.Text -> Config -> m a

  default get :: (Generic a, GFromConf (Rep a), MonadThrow m) => T.Text -> Config -> m a
  get p c = fmap to (gget p c)

instance FromConf T.Text where
  get = getString

instance FromConf Bool where
  get = getBool

instance FromConf Integer where
  get = getInteger

class GFromConf rep where
  gget :: MonadThrow m => T.Text -> Config -> m (rep a)

instance GFromConf U1 where
  gget _ _ = return U1

instance (GFromConf a, GFromConf b) => GFromConf (a :*: b) where
  gget p c = do
    x <- gget p c
    y <- gget p c
    return (x :*: y)

instance GFromConf a => GFromConf (M1 D x a) where
  gget p c = fmap M1 (gget p c)

instance GFromConf a => GFromConf (M1 C x a) where
  gget p c = fmap M1 (gget p c)

instance (GFromConf a, Selector s) => GFromConf (M1 S s a) where
  gget p c = fmap M1 (gget (p' `T.append` T.pack (selName (undefined :: M1 S s a ()))) c)
             where p' = if T.null p then p else p `T.append` "."

instance (FromConf a) => GFromConf (K1 R a) where
  gget p c = fmap K1 (get p c)

