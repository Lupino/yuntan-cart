{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (delete, get, json,
                                                       middleware, post,
                                                       scottyOptsT, settings)

import           Yuntan.Types.HasMySQL                (HasMySQL, simpleEnv)
import           Yuntan.Types.Scotty                  (ScottyH)

import           Cart
import           Cart.APIHandler
import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)

import qualified Cart.Config                          as C
import qualified Data.Yaml                            as Y

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options { getConfigFile  :: String
                       , getHost        :: String
                       , getPort        :: Int
                       , getTablePrefix :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "Cart micro server config file."
                                <> value "config.yaml")
                 <*> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Cart micro server hostname."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT"
                                <> help "Cart micro server port."
                                <> value 3000)
                 <*> strOption (long "table_prefix"
                                <> metavar "TABLE_PREFIX"
                                <> help "table prefix."
                                <> value "test")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Cart micro server"
     <> header "yuntan-cart - Cart micro server" )

program :: Options -> IO ()
program Options { getConfigFile  = confFile
                , getTablePrefix = prefix
                , getHost        = host
                , getPort        = port
                } = do
  (Right conf) <- Y.decodeFileEither confFile

  let mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig

  pool <- C.genMySQLPool mysqlConfig

  let state = stateSet (initCartState mysqlThreads) stateEmpty

  let u = simpleEnv pool prefix ()

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  _ <- runIO u state createTable
  scottyOptsT opts (runIO u state) application
  where
        runIO :: HasMySQL u => u -> StateStore -> GenHaxl u w b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: HasMySQL u => ScottyH u w ()
application = do
  middleware logStdout

  get    "/api/cart/:username/" getCartHandler
  post   "/api/cart/:username/" addProductHandler
  delete "/api/cart/:username/" removeProductHandler

  post "/api/orders/" createOrderHandler

  post "/api/orders/:orderIdOrSN/status/:status/" $ requireOrder updateOrderStatusHandler
  post "/api/orders/:orderIdOrSN/body/" $ requireOrder updateOrderBodyHandler
  post "/api/orders/:orderIdOrSN/amount/" $ requireOrder updateOrderAmountHandler

  get "/api/orders/" getOrderListHandler

  get "/api/orders/:orderIdOrSN/" $ requireOrder json
  delete "/api/orders/:orderIdOrSN/" $ requireOrder removeOrderHandler

  get  "/api/orders_by/user/:username/" getOrderListByUserNameHandler
  get  "/api/orders_by/user/:username/:orderIdOrSN/" $ requireOrder $ requireOwner json
  get  "/api/orders_by/user/:username/status/:status" getOrderListByUserNameAndStatusHandler
  get  "/api/orders_by/status/:status/" getOrderListByStatusHandler
  post "/api/orders_by/user/:username/:orderIdOrSN/status/:status/" $
    requireOrder $ requireOwner updateOrderStatusHandler
