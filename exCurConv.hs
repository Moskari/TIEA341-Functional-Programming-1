{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.Read as Read
import Data.Monoid (mconcat)

convToEur :: Text -> Maybe Double -> Maybe Double
convToEur from (Just val) = case (T.unpack from) of 
  "eur" -> Just val
  "usd" -> Just (val * 0.931723)
  "pounds" -> Just(val * 1.161618)
  otherwise -> Nothing
convToEur _ _ = Nothing

convToDest :: Text -> Maybe Double -> Maybe Double
convToDest to (Just val) = case (T.unpack to) of "eur" -> Just val; "usd" -> Just (val * 1.07328); "pounds" -> Just (val * 0.860869); otherwise -> Nothing
convToDest _ _ = Nothing

result :: Text -> Text -> Text -> Text
result from amount to = case (convToDest to . convToEur from . Just . read $ T.unpack amount) of
  Just r -> T.pack $ show r
  otherwise -> T.pack "undefined"

main = scotty 3000 $ do
  get "/convert/:amount/" $ do
    amount <- param "amount"
    from <- param "from"
    to <- param "to"

    html $ mconcat ["<h1>Convert from ", amount, " ", from, " to ",  result from amount to, " ", to, "</h1>"]
