module Cart.API
  (
    addProduct
  , getCart
  , removeProduct

  , createOrder
  , getOrderById
  , getOrderBySN
  , getOrderList
  , getOrderListByStatus
  , getOrderListByUserName
  , getOrderListByUserNameAndStatus
  , countOrder
  , countOrderByStatus
  , countOrderByUserName
  , countOrderByUserNameAndStatus
  , updateOrderStatus
  , updateOrderBody
  , updateOrderAmount
  , removeOrder

  , createTable
  ) where

import Data.Int (Int64)
import Haxl.Core (uncachedRequest, dataFetch)

import Cart.DataSource
import Cart.Types
import Cart.UserEnv (CartM)

import Yuntan.Types.ListResult (From, Size)
import Yuntan.Types.OrderBy (OrderBy)


addProduct :: UserName -> ProductID -> Int -> CartM CartID
addProduct name pid num = uncachedRequest (AddProduct name pid num)

getCart :: UserName -> CartM [Cart]
getCart name = uncachedRequest (GetCart name)

removeProduct :: UserName -> ProductID -> CartM Int64
removeProduct name pid = uncachedRequest (RemoveProduct name pid)

createTable :: CartM Int64
createTable = uncachedRequest CreateTable

createOrder :: UserName -> OrderSN -> OrderBody -> OrderAmount -> OrderStatus
            -> CartM OrderID
createOrder a b c d e = uncachedRequest (CreateOrder a b c d e)

getOrderById :: OrderID -> CartM (Maybe Order)
getOrderById a = dataFetch (GetOrderById a)

getOrderBySN :: OrderSN -> CartM (Maybe Order)
getOrderBySN a = dataFetch (GetOrderBySN a)

removeOrder :: OrderID -> CartM Int64
removeOrder a = uncachedRequest (RemoveOrder a)

updateOrderStatus :: OrderID -> OrderStatus -> CartM Int64
updateOrderStatus a b = uncachedRequest (UpdateOrderStatus a b)

updateOrderAmount :: OrderID -> OrderAmount -> CartM Int64
updateOrderAmount a b = uncachedRequest (UpdateOrderAmount a b)

updateOrderBody  :: OrderID -> OrderBody -> CartM Int64
updateOrderBody a b = uncachedRequest (UpdateOrderBody a b)

getOrderList :: From -> Size -> OrderBy -> CartM [Order]
getOrderList a b c = dataFetch (GetOrderList a b c)

getOrderListByStatus  :: OrderStatus -> From -> Size -> OrderBy
                      -> CartM [Order]
getOrderListByStatus a b c d = dataFetch (GetOrderListByStatus a b c d)

getOrderListByUserName :: UserName
                       -> From -> Size -> OrderBy -> CartM [Order]
getOrderListByUserName a b c d = dataFetch (GetOrderListByUserName a b c d)

getOrderListByUserNameAndStatus :: UserName -> OrderStatus
                                -> From -> Size -> OrderBy -> CartM [Order]
getOrderListByUserNameAndStatus a b c d e = dataFetch (GetOrderListByUserNameAndStatus a b c d e)

countOrder :: CartM Int64
countOrder = dataFetch CountOrder

countOrderByStatus :: OrderStatus -> CartM Int64
countOrderByStatus a = dataFetch (CountOrderByStatus a)

countOrderByUserName :: UserName -> CartM Int64
countOrderByUserName a = dataFetch (CountOrderByUserName a)

countOrderByUserNameAndStatus :: UserName -> OrderStatus -> CartM Int64
countOrderByUserNameAndStatus a b = dataFetch (CountOrderByUserNameAndStatus a b)
