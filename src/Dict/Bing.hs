{-# LANGUAGE OverloadedStrings #-}
module Dict.Bing where

import           Control.Monad
import qualified Data.Text as Text
import           Data.Text (Text)
import           Dict.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.HTML.DOM
import           Text.XML (Name)
import           Text.XML.Cursor

translate :: String -> IO DictItem
translate word = do
  req <- parseRequest ("https://cn.bing.com/dict/search?q=" ++ word)
  manager <- newManager tlsManagerSettings
  cur <- liftM (fromDocument . parseLBS . responseBody) $ httpLbs req manager
  let word' = Text.unpack $ Text.concat $ concat $ filter (not . null) $ cur $// (element "div" &| attributeIs "id" "headword" &.// content)
      pronounces' = map Text.unpack $ concat $ concat $ filter (not . null) $ cur $// (element "div" &| attributeIs "class" "hd_tf_lh" &/ element "div" &/ element "div" &| attributeIsNot "class" "hd_tf" &.// content)
      meanings' = concat $ concat $ filter (not . null) $ cur $// (element "div" &| attributeIs "class" "qdef" &/ element "ul" &/ element "li" &| (\li ->
                    [( Text.unpack $ Text.concat $ concat $ filter (not . null) $ li $/ element "span" &| attributeHas "class" "pos" &.// content
                     , Text.unpack $ Text.concat $ concat $ filter (not . null) $ li $/ element "span" &| attributeIs "class" "def" &/ element "span" &.// content)]))
  return $ DictItem word' pronounces' meanings'
  where
    attributeHas :: Name -> Text -> Axis
    attributeHas n t = check (any (elem t . Text.words) . attribute n)
    attributeIsNot :: Name -> Text -> Axis
    attributeIsNot n t = check (any (t /=) . attribute n)
