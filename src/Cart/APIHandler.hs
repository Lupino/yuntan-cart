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
  , removeOrderHandler
  ) where

import           Control.Monad           (void)
import           Control.Monad.Reader    (lift)

import           Cart
import           Data.Aeson              (Value (..), decode)
import           Data.Int                (Int64)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (pack)
import           Haxl.Core               (GenHaxl)
import           Network.HTTP.Types      (status403, status500)
import           Web.Scotty.Trans        (json, param, rescue)
import           Yuntan.Types.HasMySQL   (HasMySQL)
import           Yuntan.Types.ListResult (From, ListResult (..), Size)
import           Yuntan.Types.OrderBy    (OrderBy, desc)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.JSON       (unionValue)
import           Yuntan.Utils.Scotty     (err, errBadRequest, errNotFound, ok,
                                          okListResult)

-- POST /api/cart/:username/
addProductHandler :: HasMySQL u => ActionH u w ()
addProductHandler = do
  name <- param "username"
  pid <- param "product_id"
  num <- param "num"
  cartId <- lift $ addProduct name pid num
  if cartId > 0 then resultOK
                else err status500 "add product failed."

-- GET /api/cart/:username/
getCartHandler :: HasMySQL u => ActionH u w ()
getCartHandler = do
  name <- param "username"
  cart <- lift $ getCart name
  ok "result" cart

-- DELETE /api/cart/:username/
removeProductHandler :: HasMySQL u => ActionH u w ()
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
apiOrder :: HasMySQL u => ActionH u w (Maybe Order)
apiOrder = do
  name <- param "orderIdOrSN"
  order <- lift $ getOrderBySN (pack name)
  case order of
    Just o -> return $ Just o
    Nothing ->
      if isDigest name then lift (getOrderById $ read name)
                       else return Nothing

requireOrder :: HasMySQL u => (Order -> ActionH u w ()) -> ActionH u w ()
requireOrder next = do
  order <- apiOrder
  case order of
    Just o  -> next o
    Nothing -> errNotFound "Order is not found"

requireOwner :: (Order -> ActionH u w ()) -> Order -> ActionH u w ()
requireOwner next order = do
  username <- param "username"
  if orderUserName order == username then next order
                                     else err status403 "no permission"

-- POST /api/orders/
createOrderHandler :: HasMySQL u => ActionH u w ()
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
updateOrderStatusHandler :: HasMySQL u => Order -> ActionH u w ()
updateOrderStatusHandler Order{orderID = oid} = do
  st <- param "status"
  ret <- lift $ updateOrderStatus oid st
  resultOKOrErr ret "update order status failed"

-- POST /api/orders/:orderIdOrSN/body/
updateOrderBodyHandler :: HasMySQL u => Order -> ActionH u w ()
updateOrderBodyHandler Order{orderID = oid, orderBody = obody} = do
  body <- param "body"
  case decode body of
    Just ev -> void (lift $ updateOrderBody oid $ unionValue ev obody) >> resultOK
    Nothing -> errBadRequest "body filed is required."

-- POST /api/orders/:orderIdOrSN/amount/
updateOrderAmountHandler :: HasMySQL u => Order -> ActionH u w ()
updateOrderAmountHandler Order{orderID = oid} = do
  amount <- param "amount"
  ret <- lift $ updateOrderAmount oid amount
  resultOKOrErr ret "update order amount failed"

-- GET /api/orders/
getOrderListHandler :: HasMySQL u => ActionH u w ()
getOrderListHandler = resultOrderList getOrderList countOrder

-- GET /api/orders_by/status/:status/
getOrderListByStatusHandler :: HasMySQL u => ActionH u w ()
getOrderListByStatusHandler = do
  st <- param "status"
  resultOrderList (getOrderListByStatus st) (countOrderByStatus st)

-- GET /api/orders_by/user/:username/
getOrderListByUserNameHandler :: HasMySQL u => ActionH u w ()
getOrderListByUserNameHandler = do
  name <- param "username"
  resultOrderList (getOrderListByUserName name) (countOrderByUserName name)

-- GET /api/orders_by/user/:username/status/:status/
getOrderListByUserNameAndStatusHandler :: HasMySQL u => ActionH u w ()
getOrderListByUserNameAndStatusHandler = do
  name <- param "username"
  st <- param "status"
  resultOrderList (getOrderListByUserNameAndStatus name st) (countOrderByUserNameAndStatus name st)

-- DELETE /api/orders/:orderIdOrSN/
removeOrderHandler :: HasMySQL u => Order -> ActionH u w ()
removeOrderHandler Order{orderID = oid} = do
  void $ lift $ removeOrder oid
  resultOK

resultOK :: ActionH u w ()
resultOK = ok "result" ("OK" :: String)

resultOKOrErr :: Int64 -> String -> ActionH u w ()
resultOKOrErr o m = if o > 0 then resultOK
                             else err status500 m

resultOrderList :: HasMySQL u => (From -> Size -> OrderBy -> GenHaxl u w [Order]) -> GenHaxl u w Int64 -> ActionH u w ()
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
