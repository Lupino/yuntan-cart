{-# LANGUAGE OverloadedStrings #-}

module Cart.DataSource.Table
  (
    createTable
  ) where

import Database.MySQL.Simple (Connection, execute_)

import Data.Int (Int64)
import Data.String (fromString)

import Cart.Types


createCartTable :: TablePrefix -> Connection -> IO Int64
createCartTable prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_carts` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `username` varchar(128) NOT NULL,"
                                  , "  `product_id` int(10) unsigned NOT NULL,"
                                  , "  `number` int(10) unsigned NOT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `username_product` (`username`, `product_id`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createOrderTable :: TablePrefix -> Connection -> IO Int64
createOrderTable prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_orders` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `order_sn` varchar(128) NOT NULL,"
                                  , "  `username` varchar(128) NOT NULL,"
                                  , "  `body` TEXT DEFAULT NULL,"
                                  , "  `amount` int(10) unsigned NOT NULL,"
                                  , "  `status` varchar(10) NOT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `order_sn` (`order_sn`),"
                                  , "  KEY `username` (`username`),"
                                  , "  KEY `status` (`status`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTable :: TablePrefix -> Connection -> IO Int64
createTable prefix conn = sum <$> mapM (\o -> o prefix conn) [ createCartTable
                                                             , createOrderTable
                                                             ]
