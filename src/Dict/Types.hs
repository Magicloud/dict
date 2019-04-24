module Dict.Types where

data DictItem = DictItem { diWord :: String
                         , diPronounces :: [String]
                         , diMeanings :: [(String, String)] }
              deriving (Show)
