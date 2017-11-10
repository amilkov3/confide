
module Data.Confide ( loadConfig ) where

import qualified Data.Config as C
import Data.Text
import Control.Monad.Catch
import Control.Monad.IO.Class


loadConfig :: (MonadIO m, MonadThrow m) => Text -> m Config
loadConfig = C.loadConfig
