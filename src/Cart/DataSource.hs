{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Cart.DataSource (
    CartReq(..),
    initCartState
  ) where

import Data.Hashable (Hashable (..))
import Data.Typeable (Typeable)
import Haxl.Core hiding (env, fetchReq)

import Cart.DataSource.Cart
import Cart.DataSource.Order
import Cart.DataSource.Table
import Cart.Types
import Yuntan.Types.HasMySQL (HasMySQL, mysqlPool, tablePrefix)
import Yuntan.Types.ListResult (From, Size)
import Yuntan.Types.OrderBy (OrderBy)

import qualified Control.Exception (SomeException, bracket_, try)
import Data.Int (Int64)
import Data.Pool (withResource)
import Database.MySQL.Simple (Connection)

import Control.Concurrent.Async
import Control.Concurrent.QSem

-- Data source implementation.

data CartReq a where
  AddProduct :: UserName -> ProductID -> Int -> CartReq CartID
  GetCart :: UserName -> CartReq [Cart]
  RemoveProduct :: UserName -> ProductID -> CartReq Int64

  CreateOrder :: UserName -> OrderSN -> OrderBody -> OrderAmount -> OrderStatus
              -> CartReq OrderID
  GetOrderById :: OrderID -> CartReq (Maybe Order)
  GetOrderBySN :: OrderSN -> CartReq (Maybe Order)
  RemoveOrder :: OrderID -> CartReq Int64
  UpdateOrderStatus :: OrderID -> OrderStatus -> CartReq Int64
  UpdateOrderAmount :: OrderID -> OrderAmount -> CartReq Int64
  UpdateOrderBody  :: OrderID -> OrderBody -> CartReq Int64
  GetOrderList :: From -> Size -> OrderBy -> CartReq [Order]
  GetOrderListByStatus  :: OrderStatus -> From -> Size -> OrderBy
                        -> CartReq [Order]
  GetOrderListByUserName :: UserName
                         -> From -> Size -> OrderBy -> CartReq [Order]
  GetOrderListByUserNameAndStatus :: UserName -> OrderStatus
                                  -> From -> Size -> OrderBy -> CartReq [Order]
  CountOrder :: CartReq Int64
  CountOrderByStatus :: OrderStatus -> CartReq Int64
  CountOrderByUserName :: UserName -> CartReq Int64
  CountOrderByUserNameAndStatus :: UserName -> OrderStatus -> CartReq Int64

  CreateTable :: CartReq Int64

  deriving (Typeable)

deriving instance Eq (CartReq a)
instance Hashable (CartReq a) where
  hashWithSalt s (AddProduct n p k) = hashWithSalt s (0::Int, n, p, k)
  hashWithSalt s (GetCart n) = hashWithSalt s (1::Int, n)
  hashWithSalt s (RemoveProduct n p) = hashWithSalt s (2::Int, n, p)

  hashWithSalt s (CreateOrder a b c d e) = hashWithSalt s (10::Int, a, b, c, d, e)
  hashWithSalt s (GetOrderById a) = hashWithSalt s (11::Int, a)
  hashWithSalt s (GetOrderBySN a) = hashWithSalt s (12::Int, a)
  hashWithSalt s (RemoveOrder a) = hashWithSalt s (13::Int, a)
  hashWithSalt s (UpdateOrderStatus a b) = hashWithSalt s (14::Int, a, b)
  hashWithSalt s (UpdateOrderAmount a b) = hashWithSalt s (15::Int, a, b)
  hashWithSalt s (UpdateOrderBody a b) = hashWithSalt s (16::Int, a, b)
  hashWithSalt s (GetOrderList a b c) = hashWithSalt s (17::Int, a, b, c)
  hashWithSalt s (GetOrderListByStatus a b c d) = hashWithSalt s (18::Int, a, b, c, d)
  hashWithSalt s (GetOrderListByUserName a b c d) = hashWithSalt s (19::Int, a, b, c, d)
  hashWithSalt s (GetOrderListByUserNameAndStatus a b c d e) = hashWithSalt s (20::Int, a, b, c, d, e)
  hashWithSalt s CountOrder = hashWithSalt s (21::Int)
  hashWithSalt s (CountOrderByStatus a) = hashWithSalt s (22::Int, a)
  hashWithSalt s (CountOrderByUserName a) = hashWithSalt s (23::Int, a)
  hashWithSalt s (CountOrderByUserNameAndStatus a b) = hashWithSalt s (24::Int, a, b)

  hashWithSalt s CreateTable = hashWithSalt s (3::Int)

deriving instance Show (CartReq a)
instance ShowP CartReq where showp = show

instance StateKey CartReq where
  data State CartReq = CartState { numThreads :: Int }

instance DataSourceName CartReq where
  dataSourceName _ = "CartDataSource"

instance HasMySQL u => DataSource u CartReq where
  fetch = yuntanFetch

yuntanFetch
  :: HasMySQL u
  => State CartReq
  -> Flags
  -> u
  -> [BlockedFetch CartReq]
  -> PerformFetch

yuntanFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch CartReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem)
  $ withResource pool
  $ fetchSync req prefix

  where pool   = mysqlPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch CartReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: CartReq a -> TablePrefix -> Connection -> IO a
fetchReq (AddProduct n p k) = addProduct n  p  k
fetchReq (GetCart n) = getCart n
fetchReq (RemoveProduct n p) = removeProduct n  p

fetchReq (CreateOrder a b c d e) = createOrder a b c d e
fetchReq (GetOrderById a) = getOrderById a
fetchReq (GetOrderBySN a) = getOrderBySN a
fetchReq (RemoveOrder a) = removeOrder a
fetchReq (UpdateOrderStatus a b) = updateOrderStatus a b
fetchReq (UpdateOrderAmount a b) = updateOrderAmount a b
fetchReq (UpdateOrderBody a b) = updateOrderBody a b
fetchReq (GetOrderList a b c) = getOrderList a b c
fetchReq (GetOrderListByStatus a b c d) = getOrderListByStatus a b c d
fetchReq (GetOrderListByUserName a b c d) = getOrderListByUserName a b c d
fetchReq (GetOrderListByUserNameAndStatus a b c d e) = getOrderListByUserNameAndStatus a b c d e
fetchReq CountOrder = countOrder
fetchReq (CountOrderByStatus a) = countOrderByStatus a
fetchReq (CountOrderByUserName a) = countOrderByUserName a
fetchReq (CountOrderByUserNameAndStatus a b) = countOrderByUserNameAndStatus a b

fetchReq CreateTable = createTable

initCartState :: Int -> State CartReq
initCartState = CartState
