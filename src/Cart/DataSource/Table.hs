{-# LANGUAGE OverloadedStrings #-}

module Cart.DataSource.Table
  ( mergeData
  , carts
  , orders
  ) where

import           Data.Int            (Int64)
import           Database.PSQL.Types (PSQL, TableName, VersionList, createIndex,
                                      createTable, mergeDatabase)

carts :: TableName
carts = "carts"

orders :: TableName
orders = "orders"

createCartTable :: PSQL Int64
createCartTable = createTable carts
  [ "id SERIAL PRIMARY KEY"
  , "username VARCHAR(128) NOT NULL"
  , "product_id INT DEFAULT '0'"
  , "number INT DEFAULT '0'"
  , "created_at INT DEFAULT '0'"
  ]


createOrderTable :: PSQL Int64
createOrderTable = createTable orders
  [ "id SERIAL PRIMARY KEY"
  , "order_sn VARCHAR(128) NOT NULL"
  , "username VARCHAR(128) NOT NULL"
  , "body JSON"
  , "amount INT DEFAULT '0'"
  , "status VARCHAR(10) NOT NULL"
  , "created_at INT DEFAULT '0'"
  ]

versionList :: VersionList Int64
versionList =
  [ (1, [ createCartTable
        , createIndex True carts "username_product" ["username", "product_id"]
        , createOrderTable
        , createIndex True orders "order_sn" ["order_sn"]
        , createIndex False orders "username" ["username"]
        , createIndex False orders "status" ["status"]
        ])
  ]

mergeData :: PSQL ()
mergeData = mergeDatabase versionList
