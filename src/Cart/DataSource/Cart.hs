{-# LANGUAGE OverloadedStrings #-}

module Cart.DataSource.Cart
  ( addProduct
  , getCart
  , removeProduct
  ) where

import           Cart.DataSource.Table  (carts)
import           Cart.Types
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.String            (fromString)
import           Data.UnixTime
import           Database.PSQL.Types    (Only (..), PSQL, delete, insertRet,
                                         none, select, selectOneOnly, update)

addProduct :: UserName -> ProductID -> Int -> PSQL CartID
addProduct name pid num = do
  mcartId <- checkProduct name pid
  case mcartId of
    Just cartId -> do
      void $ updateProduct cartId num
      return cartId
    Nothing -> do
      t <- liftIO getUnixTime
      insertRet carts
        ["username", "product_id", "number", "created_at"] "id"
        (name, pid, num, show $ toEpochTime t) 0

updateProduct :: CartID -> Int -> PSQL Int64
updateProduct cartId num =
  update carts [fromString $ "number = number " ++ op ++ " ?"] "id = ?" (show num', cartId)
  where op = if num > 0 then "+" else "-"
        num' = if num > 0 then num else - num

checkProduct :: UserName -> ProductID -> PSQL (Maybe Int64)
checkProduct name pid =
  selectOneOnly carts "id" "username = ? AND product_id = ?" (name, pid)

getCart :: UserName -> PSQL [Cart]
getCart name =
  select carts ["*"] "username = ?" (Only name) 0 500 none

removeProduct :: UserName -> ProductID -> PSQL Int64
removeProduct name pid =
  delete carts "username = ? AND product_id = ?" (name, pid)
