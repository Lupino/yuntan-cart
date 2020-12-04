{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cart.Types
  ( Cart (..)
  , CartID
  , ProductID
  , UserName
  , CreatedAt
  , Order (..)
  , OrderID
  , OrderSN
  , OrderBody
  , OrderAmount
  , OrderStatus
  ) where

import           Data.Aeson          (ToJSON (..), Value (..), object, (.=))
import           Data.Int            (Int64)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import           Database.PSQL.Types (FromRow (..), field)

type CartID      = Int64
type ProductID   = Int64
type UserName    = Text
type CreatedAt   = Int64

type OrderID      = Int64
type OrderSN      = Text
type OrderBody    = Value
type OrderAmount  = Int
type OrderStatus  = Text

data Cart = Cart
  { cartID        :: CartID
  , cartUserName  :: UserName
  , cartProductID :: ProductID
  , cartNumber    :: Int
  , cartCreatedAt :: CreatedAt
  }
  deriving (Show)

instance FromRow Cart where
  fromRow = do
    cartID          <- field
    cartUserName       <- field
    cartProductID     <- field
    cartNumber     <- field
    cartCreatedAt       <- field
    return Cart {..}

instance ToJSON Cart where
  toJSON Cart {..} = object
    [ "id"         .= cartID
    , "username"   .= cartUserName
    , "product_id" .= cartProductID
    , "number"     .= cartNumber
    , "created_at" .= cartCreatedAt
    ]


data Order = Order
  { orderID        :: OrderID
  , orderSN        :: OrderSN
  , orderUserName  :: UserName
  , orderBody      :: OrderBody
  , orderAmount    :: OrderAmount
  , orderStatus    :: OrderStatus
  , orderCreatedAt :: CreatedAt
  }
  deriving (Show)

instance FromRow Order where
  fromRow = do
    orderID        <- field
    orderSN        <- field
    orderUserName  <- field
    orderBody      <- fromMaybe Null <$> field
    orderAmount    <- field
    orderStatus    <- field
    orderCreatedAt <- field
    return Order {..}

instance ToJSON Order where
  toJSON Order {..} = object
    [ "id"         .= orderID
    , "order_sn"   .= orderSN
    , "username"   .= orderUserName
    , "body"       .= orderBody
    , "amount"     .= orderAmount
    , "status"     .= orderStatus
    , "created_at" .= orderCreatedAt
    ]
