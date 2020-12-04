module Cart.API
  ( addProduct
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

  , mergeData
  ) where

import           Cart.DataSource
import           Cart.Types
import           Data.Int            (Int64)
import           Database.PSQL.Types (From, HasPSQL, OrderBy, Size)
import           Haxl.Core           (GenHaxl, dataFetch, uncachedRequest)


addProduct :: HasPSQL u => UserName -> ProductID -> Int -> GenHaxl u w CartID
addProduct name pid num = uncachedRequest (AddProduct name pid num)

getCart :: HasPSQL u => UserName -> GenHaxl u w [Cart]
getCart name = uncachedRequest (GetCart name)

removeProduct :: HasPSQL u => UserName -> ProductID -> GenHaxl u w Int64
removeProduct name pid = uncachedRequest (RemoveProduct name pid)

mergeData :: HasPSQL u => GenHaxl u w ()
mergeData = uncachedRequest MergeData

createOrder :: HasPSQL u => UserName -> OrderSN -> OrderBody -> OrderAmount -> OrderStatus
            -> GenHaxl u w OrderID
createOrder a b c d e = uncachedRequest (CreateOrder a b c d e)

getOrderById :: HasPSQL u => OrderID -> GenHaxl u w (Maybe Order)
getOrderById a = dataFetch (GetOrderById a)

getOrderBySN :: HasPSQL u => OrderSN -> GenHaxl u w (Maybe Order)
getOrderBySN a = dataFetch (GetOrderBySN a)

removeOrder :: HasPSQL u => OrderID -> GenHaxl u w Int64
removeOrder a = uncachedRequest (RemoveOrder a)

updateOrderStatus :: HasPSQL u => OrderID -> OrderStatus -> GenHaxl u w Int64
updateOrderStatus a b = uncachedRequest (UpdateOrderStatus a b)

updateOrderAmount :: HasPSQL u => OrderID -> OrderAmount -> GenHaxl u w Int64
updateOrderAmount a b = uncachedRequest (UpdateOrderAmount a b)

updateOrderBody  :: HasPSQL u => OrderID -> OrderBody -> GenHaxl u w Int64
updateOrderBody a b = uncachedRequest (UpdateOrderBody a b)

getOrderList :: HasPSQL u => From -> Size -> OrderBy -> GenHaxl u w [Order]
getOrderList a b c = dataFetch (GetOrderList a b c)

getOrderListByStatus  :: HasPSQL u => OrderStatus -> From -> Size -> OrderBy
                      -> GenHaxl u w [Order]
getOrderListByStatus a b c d = dataFetch (GetOrderListByStatus a b c d)

getOrderListByUserName :: HasPSQL u => UserName
                       -> From -> Size -> OrderBy -> GenHaxl u w [Order]
getOrderListByUserName a b c d = dataFetch (GetOrderListByUserName a b c d)

getOrderListByUserNameAndStatus :: HasPSQL u => UserName -> OrderStatus
                                -> From -> Size -> OrderBy -> GenHaxl u w [Order]
getOrderListByUserNameAndStatus a b c d e = dataFetch (GetOrderListByUserNameAndStatus a b c d e)

countOrder :: HasPSQL u => GenHaxl u w Int64
countOrder = dataFetch CountOrder

countOrderByStatus :: HasPSQL u => OrderStatus -> GenHaxl u w Int64
countOrderByStatus a = dataFetch (CountOrderByStatus a)

countOrderByUserName :: HasPSQL u => UserName -> GenHaxl u w Int64
countOrderByUserName a = dataFetch (CountOrderByUserName a)

countOrderByUserNameAndStatus :: HasPSQL u => UserName -> OrderStatus -> GenHaxl u w Int64
countOrderByUserNameAndStatus a b = dataFetch (CountOrderByUserNameAndStatus a b)
