module App.Routes where

import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Prelude (($), (<$>))
import Pux.Router (end, router, lit, int)
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

data Route = Home | Posts Int | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Posts <$> (lit "posts" *> int) <* end
