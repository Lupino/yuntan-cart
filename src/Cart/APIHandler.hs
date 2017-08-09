{-# LANGUAGE OverloadedStrings #-}
module Cart.APIHandler
  (
    addProductHandler
  , getCartHandler
  , removeProductHandler

  , createOrderHandler
  , getOrderListHandler
  , getOrderListByStatusHandler
  , getOrderListByUserNameHandler
  , getOrderListByUserNameAndStatusHandler
  , requireOwner
  , requireOrder
  , updateOrderStatusHandler
  , updateOrderBodyHandler
  , updateOrderAmountHandler
  ) where

import Control.Monad (void)
import Control.Monad.Reader (lift)

import Cart
import Dispatch.Utils.Scotty (err, ok, errNotFound, errBadRequest, okListResult)
import Network.HTTP.Types (status500, status403)
import Web.Scotty.Trans (param, json, rescue)
import Dispatch.Types.ListResult (From, ListResult (..), Size)
import Dispatch.Types.OrderBy (desc, OrderBy)
import Dispatch.Utils.JSON (unionValue)
import Data.Text (pack)
import Data.Aeson (decode, Value (..))
import Data.Maybe (fromMaybe)
import Data.Int (Int64)

-- POST /api/cart/:username/
addProductHandler :: ActionM ()
addProductHandler = do
  name <- param "username"
  pid <- param "product_id"
  num <- param "num"
  cartId <- lift $ addProduct name pid num
  if cartId > 0 then resultOK
                else err status500 "add product failed."

-- GET /api/cart/:username/
getCartHandler :: ActionM ()
getCartHandler = do
  name <- param "username"
  cart <- lift $ getCart name
  ok "result" cart

-- DELETE /api/cart/:username/
removeProductHandler :: ActionM ()
removeProductHandler = do
  name <- param "username"
  pid <- param "product_id"
  void . lift $ removeProduct name pid
  resultOK

isDigest :: String -> Bool
isDigest (x:xs) | x `elem` ['0'..'9'] = isDigest xs
                | otherwise           = False

isDigest [] = True

-- :orderIdOrSN
apiOrder :: ActionM (Maybe Order)
apiOrder = do
  name <- param "orderIdOrSN"
  order <- lift $ getOrderBySN (pack name)
  case order of
    Just o -> return $ Just o
    Nothing -> do
      if isDigest name then lift (getOrderById $ read name)
                       else return Nothing

requireOrder :: (Order -> ActionM ()) -> ActionM ()
requireOrder next = do
  order <- apiOrder
  case order of
    Just o -> next o
    Nothing -> errNotFound "Order is not found"

requireOwner :: (Order -> ActionM ()) -> Order -> ActionM ()
requireOwner next order = do
  username <- param "username"
  if orderUserName order == username then next order
                                     else err status403 "no permission"

-- POST /api/orders/
createOrderHandler :: ActionM ()
createOrderHandler = do
  username <- param "username"
  amount <- param "amount"
  body <- fromMaybe Null . decode <$> param "body"
  sn <- param "order_sn"
  st <- param "status"

  if isDigest sn then errBadRequest "order_sn is invalid"
                 else do

    orderId <- lift $ createOrder username (pack sn) body amount st
    json =<< lift (getOrderById orderId)

-- POST /api/orders/:orderIdOrSN/status/:status/
-- POST /api/orders_by/user/:username/:orderIdOrSN/status/:status/
updateOrderStatusHandler :: Order -> ActionM ()
updateOrderStatusHandler (Order { orderID = oid }) = do
  st <- param "status"
  ret <- lift $ updateOrderStatus oid st
  resultOKOrErr ret "update order status failed"

-- POST /api/orders/:orderIdOrSN/body/
updateOrderBodyHandler :: Order -> ActionM ()
updateOrderBodyHandler (Order { orderID = oid, orderBody = obody }) = do
  body <- param "body"
  case (decode body) of
    Just ev -> void (lift $ updateOrderBody oid $ unionValue ev obody) >> resultOK
    Nothing -> errBadRequest "body filed is required."

-- POST /api/orders/:orderIdOrSN/amount/
updateOrderAmountHandler :: Order -> ActionM ()
updateOrderAmountHandler (Order { orderID = oid }) = do
  amount <- param "amount"
  ret <- lift $ updateOrderAmount oid amount
  resultOKOrErr ret "update order amount failed"

-- GET /api/orders/
getOrderListHandler :: ActionM ()
getOrderListHandler = resultOrderList getOrderList countOrder

-- GET /api/orders_by/status/:status/
getOrderListByStatusHandler :: ActionM ()
getOrderListByStatusHandler = do
  st <- param "status"
  resultOrderList (getOrderListByStatus st) (countOrderByStatus st)

-- GET /api/orders_by/user/:username/
getOrderListByUserNameHandler :: ActionM ()
getOrderListByUserNameHandler = do
  name <- param "username"
  resultOrderList (getOrderListByUserName name) (countOrderByUserName name)

-- GET /api/orders_by/user/:username/status/:status/
getOrderListByUserNameAndStatusHandler :: ActionM ()
getOrderListByUserNameAndStatusHandler = do
  name <- param "username"
  st <- param "status"
  resultOrderList (getOrderListByUserNameAndStatus name st) (countOrderByUserNameAndStatus name st)

resultOK :: ActionM ()
resultOK = ok "result" ("OK" :: String)

resultOKOrErr :: Int64 -> String -> ActionM ()
resultOKOrErr o m = if o > 0 then resultOK
                             else err status500 m

resultOrderList :: (From -> Size -> OrderBy -> CartM [Order]) -> CartM Int64 -> ActionM ()
resultOrderList getList count = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  total <- lift count
  orders <- lift $ getList from size $ desc "id"

  okListResult "orders" ListResult { getFrom   = from
                                   , getSize   = size
                                   , getTotal  = total
                                   , getResult = orders
                                   }
