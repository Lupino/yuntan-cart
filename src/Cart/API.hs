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
import Haxl.Core (GenHaxl, dataFetch, uncachedRequest)
import Yuntan.Types.HasMySQL (HasMySQL)

import Cart.DataSource
import Cart.Types

import Yuntan.Types.ListResult (From, Size)
import Yuntan.Types.OrderBy (OrderBy)


addProduct :: HasMySQL u => UserName -> ProductID -> Int -> GenHaxl u CartID
addProduct name pid num = uncachedRequest (AddProduct name pid num)

getCart :: HasMySQL u => UserName -> GenHaxl u [Cart]
getCart name = uncachedRequest (GetCart name)

removeProduct :: HasMySQL u => UserName -> ProductID -> GenHaxl u Int64
removeProduct name pid = uncachedRequest (RemoveProduct name pid)

createTable :: HasMySQL u => GenHaxl u Int64
createTable = uncachedRequest CreateTable

createOrder :: HasMySQL u => UserName -> OrderSN -> OrderBody -> OrderAmount -> OrderStatus
            -> GenHaxl u OrderID
createOrder a b c d e = uncachedRequest (CreateOrder a b c d e)

getOrderById :: HasMySQL u => OrderID -> GenHaxl u (Maybe Order)
getOrderById a = dataFetch (GetOrderById a)

getOrderBySN :: HasMySQL u => OrderSN -> GenHaxl u (Maybe Order)
getOrderBySN a = dataFetch (GetOrderBySN a)

removeOrder :: HasMySQL u => OrderID -> GenHaxl u Int64
removeOrder a = uncachedRequest (RemoveOrder a)

updateOrderStatus :: HasMySQL u => OrderID -> OrderStatus -> GenHaxl u Int64
updateOrderStatus a b = uncachedRequest (UpdateOrderStatus a b)

updateOrderAmount :: HasMySQL u => OrderID -> OrderAmount -> GenHaxl u Int64
updateOrderAmount a b = uncachedRequest (UpdateOrderAmount a b)

updateOrderBody  :: HasMySQL u => OrderID -> OrderBody -> GenHaxl u Int64
updateOrderBody a b = uncachedRequest (UpdateOrderBody a b)

getOrderList :: HasMySQL u => From -> Size -> OrderBy -> GenHaxl u [Order]
getOrderList a b c = dataFetch (GetOrderList a b c)

getOrderListByStatus  :: HasMySQL u => OrderStatus -> From -> Size -> OrderBy
                      -> GenHaxl u [Order]
getOrderListByStatus a b c d = dataFetch (GetOrderListByStatus a b c d)

getOrderListByUserName :: HasMySQL u => UserName
                       -> From -> Size -> OrderBy -> GenHaxl u [Order]
getOrderListByUserName a b c d = dataFetch (GetOrderListByUserName a b c d)

getOrderListByUserNameAndStatus :: HasMySQL u => UserName -> OrderStatus
                                -> From -> Size -> OrderBy -> GenHaxl u [Order]
getOrderListByUserNameAndStatus a b c d e = dataFetch (GetOrderListByUserNameAndStatus a b c d e)

countOrder :: HasMySQL u => GenHaxl u Int64
countOrder = dataFetch CountOrder

countOrderByStatus :: HasMySQL u => OrderStatus -> GenHaxl u Int64
countOrderByStatus a = dataFetch (CountOrderByStatus a)

countOrderByUserName :: HasMySQL u => UserName -> GenHaxl u Int64
countOrderByUserName a = dataFetch (CountOrderByUserName a)

countOrderByUserNameAndStatus :: HasMySQL u => UserName -> OrderStatus -> GenHaxl u Int64
countOrderByUserNameAndStatus a b = dataFetch (CountOrderByUserNameAndStatus a b)
