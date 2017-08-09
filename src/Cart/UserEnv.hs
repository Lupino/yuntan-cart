{-# LANGUAGE OverloadedStrings #-}

module Cart.UserEnv
  (
    UserEnv (..)
  , ActionM
  , ScottyM
  , CartM
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Pool (Pool)
import Data.Text.Lazy (Text)
import Database.MySQL.Simple (Connection)
import Haxl.Core (GenHaxl)
import Haxl.Core.Monad (unsafeLiftIO)
import Web.Scotty.Trans (ActionT, ScottyT)

data UserEnv = UserEnv { mySQLPool   :: Pool Connection
                       , tablePrefix :: String
                       }

type CartM = GenHaxl UserEnv

instance MonadIO (GenHaxl u) where
  liftIO = unsafeLiftIO

type ActionM a = ActionT Text CartM a
type ScottyM a = ScottyT Text CartM a
