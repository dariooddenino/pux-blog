module App.Routes where

import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Prelude (($), (<$>))
import Pux.Router (end, router, lit, int)
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>), (<*>))

data Act = View | Edit

data Route = Home | Posts Act Int | AddPost | NotFound


match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Posts View <$> (lit "posts" *> int) <* end
  <|>
  Posts Edit <$> (lit "posts" *> int <* lit "edit") <* end
  <|>
  AddPost <$ (lit "posts" <* lit "create") <* end
