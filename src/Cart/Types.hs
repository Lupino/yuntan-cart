{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cart.Types
  (
    Cart (..)
  , CartID
  , ProductID
  , UserName
  , CreatedAt
  , TablePrefix
  , Order (..)
  , OrderID
  , OrderSN
  , OrderBody
  , OrderAmount
  , OrderStatus
  ) where

import Database.MySQL.Simple.QueryResults (QueryResults, convertError, convertResults)
import Database.MySQL.Simple.Result (convert)

import Data.Aeson (ToJSON (..), Value (..), decodeStrict, object, (.=))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

type CartID      = Int64
type ProductID   = Int64
type UserName    = Text
type CreatedAt   = Int64
type TablePrefix = String

type OrderID      = Int64
type OrderSN      = Text
type OrderBody    = Value
type OrderAmount  = Int
type OrderStatus  = Text

data Cart = Cart { cartID        :: CartID
                 , cartUserName  :: UserName
                 , cartProductID :: ProductID
                 , cartNumber    :: Int
                 , cartCreatedAt :: CreatedAt
                 }
  deriving (Show)

instance QueryResults Cart where
  convertResults [fa, fb, fc, fd, fe]
                 [va, vb, vc, vd, ve] = Cart {..}
    where !cartID        = convert fa va
          !cartUserName  = convert fb vb
          !cartProductID = convert fc vc
          !cartNumber    = convert fd vd
          !cartCreatedAt = convert fe ve
  convertResults fs vs  = convertError fs vs 2

instance ToJSON Cart where
  toJSON Cart {..} = object [ "id"         .= cartID
                            , "username"   .= cartUserName
                            , "product_id" .= cartProductID
                            , "number"     .= cartNumber
                            , "created_at" .= cartCreatedAt
                            ]


data Order = Order { orderID        :: OrderID
                   , orderSN        :: OrderSN
                   , orderUserName  :: UserName
                   , orderBody      :: OrderBody
                   , orderAmount    :: OrderAmount
                   , orderStatus    :: OrderStatus
                   , orderCreatedAt :: CreatedAt
                   }
  deriving (Show)

instance QueryResults Order where
  convertResults [fa, fb, fc, fd, fe, ff, fg]
                 [va, vb, vc, vd, ve, vf, vg] = Order {..}
    where !orderID        = convert fa va
          !orderSN        = convert fb vb
          !orderUserName  = convert fc vc
          !orderBody      = fromMaybe Null . decodeStrict $ fromMaybe "{}" vd
          !orderAmount    = convert fe ve
          !orderStatus    = convert ff vf
          !orderCreatedAt = convert fg vg
  convertResults fs vs  = convertError fs vs 2

instance ToJSON Order where
  toJSON Order {..} = object [ "id"         .= orderID
                             , "order_sn"   .= orderSN
                             , "username"   .= orderUserName
                             , "body"       .= orderBody
                             , "amount"     .= orderAmount
                             , "status"     .= orderStatus
                             , "created_at" .= orderCreatedAt
                             ]
