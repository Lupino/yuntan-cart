{-# LANGUAGE OverloadedStrings #-}

module Cart.DataSource.Order
  ( createOrder
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
  ) where

import           Cart.DataSource.Table  (orders)
import           Cart.Types
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (encode)
import           Data.Int               (Int64)
import           Data.UnixTime
import           Database.PSQL.Types    (From, Only (..), OrderBy, PSQL, Size,
                                         count, count_, delete, insertRet,
                                         select, selectOne, select_, update)

createOrder :: UserName
            -> OrderSN
            -> OrderBody
            -> OrderAmount
            -> OrderStatus
            -> PSQL OrderID
createOrder name sn body amount status = do
   t <- liftIO getUnixTime
   insertRet orders
     ["username", "order_sn", "body", "amount", "status", "created_at"] "id"
     (name, sn, encode body, amount, status, show $ toEpochTime t) 0

getOrderById :: OrderID -> PSQL (Maybe Order)
getOrderById orderId = selectOne orders ["*"] "id = ?" (Only orderId)

getOrderBySN :: OrderSN -> PSQL (Maybe Order)
getOrderBySN sn = selectOne orders ["*"] "order_sn = ?" (Only sn)

removeOrder :: OrderID -> PSQL Int64
removeOrder orderId = delete orders "id = ?" (Only orderId)

updateOrderStatus :: OrderID -> OrderStatus -> PSQL Int64
updateOrderStatus orderId status =
  update orders ["status"] "id = ?" (status, orderId)

updateOrderAmount :: OrderID -> OrderAmount -> PSQL Int64
updateOrderAmount orderId amount =
  update orders ["amount"] "id = ?" (amount, orderId)

updateOrderBody :: OrderID -> OrderBody -> PSQL Int64
updateOrderBody orderId body =
  update orders ["body"] "id = ?" (encode body, orderId)

getOrderList :: From -> Size -> OrderBy -> PSQL [Order]
getOrderList = select_ orders ["*"]

getOrderListByStatus :: OrderStatus -> From -> Size -> OrderBy -> PSQL [Order]
getOrderListByStatus status = select orders ["*"] "status = ?" (Only status)

getOrderListByUserName :: UserName -> From -> Size -> OrderBy -> PSQL [Order]
getOrderListByUserName name = select orders ["*"] "username = ?" (Only name)

getOrderListByUserNameAndStatus
  :: UserName -> OrderStatus -> From -> Size -> OrderBy -> PSQL [Order]
getOrderListByUserNameAndStatus name status =
  select orders ["*"] "username = ? AND status = ?" (name, status)

countOrder :: PSQL Int64
countOrder = count_ orders

countOrderByStatus :: OrderStatus -> PSQL Int64
countOrderByStatus status = count orders "status = ?" (Only status)

countOrderByUserName :: UserName -> PSQL Int64
countOrderByUserName name = count orders "username = ?" (Only name)

countOrderByUserNameAndStatus :: UserName -> OrderStatus -> PSQL Int64
countOrderByUserNameAndStatus name status =
  count orders "username = ? AND status = ?" (name, status)
