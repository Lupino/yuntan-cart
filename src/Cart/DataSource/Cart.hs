{-# LANGUAGE OverloadedStrings #-}

module Cart.DataSource.Cart
  (
    addProduct
  , getCart
  , removeProduct
  ) where

import Control.Monad (void)
import Database.MySQL.Simple (Connection, Only (..), execute, insertID, query)

import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.UnixTime

import Cart.Types

addProduct :: UserName -> ProductID -> Int -> TablePrefix -> Connection -> IO CartID
addProduct name pid num prefix conn = do
  cartId <- checkProduct name pid prefix conn
  if cartId > 0 then do
    void $ updateProduct cartId num prefix conn
    return cartId
  else do
    t <- getUnixTime
    void $ execute conn insertSQL (name, pid, num, show $ toEpochTime t)
    fromIntegral <$> insertID conn

  where insertSQL = fromString $ concat [ "INSERT INTO `", prefix, "_carts` "
                                        , "(`username`, `product_id`, `number`, `created_at`)"
                                        , " VALUES "
                                        , "(?, ?, ?, ?)"
                                        ]

updateProduct :: CartID -> Int -> TablePrefix -> Connection -> IO Int64
updateProduct cartId num prefix conn = execute conn updateSQL (Only cartId)
  where updateSQL = fromString $ concat [ "UPDATE `", prefix, "_carts` "
                                        , "SET `number` = `number` ", op, show num', " "
                                        , "WHERE `id` = ?"
                                        ]

        op = if num > 0 then "+" else "-"
        num' = if num > 0 then num else - num

checkProduct :: UserName -> ProductID -> TablePrefix -> Connection -> IO Int64
checkProduct name pid prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn checkSQL (name, pid)

  where checkSQL = fromString $ concat [ "SELECT `id` FROM `", prefix, "_carts` "
                                       , "WHERE `username` = ? AND `product_id` = ?"
                                       ]

getCart :: UserName -> TablePrefix -> Connection -> IO [Cart]
getCart name prefix conn = query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_carts` WHERE `username`=?"]

removeProduct :: UserName -> ProductID -> TablePrefix -> Connection -> IO Int64
removeProduct name pid prefix conn = execute conn sql (name, pid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_carts` "
                                  , "WHERE `username` = ? AND `product_id` = ?"
                                  ]
