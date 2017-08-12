{-# LANGUAGE OverloadedStrings #-}

module Cart.DataSource.Order
  (
    createOrder
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

import Control.Monad (void)
import Database.MySQL.Simple (Connection, Only (..), execute, insertID, query
                             , query_)

import Data.Aeson (encode)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.UnixTime

import Yuntan.Types.ListResult (From, Size)
import Yuntan.Types.OrderBy (OrderBy)

import Cart.Types

createOrder :: UserName
            -> OrderSN
            -> OrderBody
            -> OrderAmount
            -> OrderStatus
            -> TablePrefix -> Connection
            -> IO OrderID
createOrder name sn body amount status prefix conn = do
  t <- getUnixTime
  void $ execute conn insertSQL (name, sn, encode body, amount, status, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where insertSQL = fromString $ concat [ "INSERT INTO `", prefix, "_orders` "
                                        , "(`username`, `order_sn`, `body`, `amount`, `status`, `created_at`)"
                                        , " VALUES "
                                        , "(?, ?, ?, ?, ?, ?)"
                                        ]

getOrderById :: OrderID -> TablePrefix -> Connection -> IO (Maybe Order)
getOrderById orderId prefix conn = listToMaybe <$> query conn sql (Only orderId)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_orders` "
                                  , "WHERE `id` = ?"
                                  ]

getOrderBySN :: OrderSN -> TablePrefix -> Connection -> IO (Maybe Order)
getOrderBySN sn prefix conn = listToMaybe <$> query conn sql (Only sn)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_orders` "
                                  , "WHERE `order_sn` = ?"
                                  ]

removeOrder :: OrderID -> TablePrefix -> Connection -> IO Int64
removeOrder orderId prefix conn = execute conn sql (Only orderId)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_orders` "
                                  , "WHERE `id` = ?"
                                  ]

updateOrderStatus :: OrderID -> OrderStatus -> TablePrefix -> Connection -> IO Int64
updateOrderStatus orderId status prefix conn = execute conn sql (status, orderId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_orders` "
                                  , "SET `status` = ? "
                                  , "WHERE `id` = ?"
                                  ]

updateOrderAmount :: OrderID -> OrderAmount -> TablePrefix -> Connection -> IO Int64
updateOrderAmount orderId amount prefix conn = execute conn sql (amount, orderId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_orders` "
                                  , "SET `amount` = ? "
                                  , "WHERE `id` = ?"
                                  ]

updateOrderBody :: OrderID -> OrderBody -> TablePrefix -> Connection -> IO Int64
updateOrderBody orderId body prefix conn = execute conn sql (encode body, orderId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_orders` "
                                  , "SET `body` = ? "
                                  , "WHERE `id` = ?"
                                  ]

getOrderList :: From -> Size -> OrderBy
             -> TablePrefix -> Connection
             -> IO [Order]
getOrderList from size o prefix conn = query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_orders` ", show o, " LIMIT ?,?" ]

getOrderListByStatus :: OrderStatus
                     -> From -> Size -> OrderBy
                     -> TablePrefix -> Connection
                     -> IO [Order]
getOrderListByStatus status from size o prefix conn =
  query conn sql (status, from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_orders` "
                                  , "WHERE `status` = ? "
                                  , show o, " LIMIT ?,?"
                                  ]

getOrderListByUserName :: UserName
                       -> From -> Size -> OrderBy
                       -> TablePrefix -> Connection
                       -> IO [Order]
getOrderListByUserName name from size o prefix conn =
  query conn sql (name, from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_orders` "
                                  , "WHERE `username` = ? "
                                  , show o, " LIMIT ?,?"
                                  ]

getOrderListByUserNameAndStatus :: UserName
                                -> OrderStatus
                                -> From -> Size -> OrderBy
                                -> TablePrefix -> Connection
                                -> IO [Order]
getOrderListByUserNameAndStatus name status from size o prefix conn =
  query conn sql (name, status, from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_orders` "
                                  , "WHERE `username` = ? AND `status` = ? "
                                  , show o, " LIMIT ?,?"
                                  ]

countOrder :: TablePrefix -> Connection -> IO Int64
countOrder prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_orders` " ]

countOrderByStatus :: OrderStatus -> TablePrefix -> Connection -> IO Int64
countOrderByStatus status prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only status)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_orders` "
                                  , "WHERE `status` = ?"
                                  ]

countOrderByUserName :: UserName -> TablePrefix -> Connection -> IO Int64
countOrderByUserName name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_orders` "
                                  , "WHERE `username` = ?"
                                  ]

countOrderByUserNameAndStatus :: UserName -> OrderStatus -> TablePrefix -> Connection -> IO Int64
countOrderByUserNameAndStatus name status prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (name, status)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_orders` "
                                  , "WHERE `username` = ? AND `status` = ?"
                                  ]
