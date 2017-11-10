
module Data.Confide ( loadConfig ) where

import qualified Data.Config as C
import Data.Text
import Control.Monad.Catch
import Control.Monad.IO.Class

loadConfig :: (MonadIO m, MonadThrow m) => String -> m C.Config
loadConfig = C.loadConfig
