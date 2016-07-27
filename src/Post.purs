module App.Post where

import Prelude
import Data.Either (Either(..), either)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, text)
import Network.HTTP.Affjax (AJAX, get, post, put)
import Control.Monad.Aff (attempt)


newtype Post = Post
 { id :: Int
 , userId :: Int
 , title :: String
 -- , body :: String
 }

type State =
  { post :: Post
  , status :: String
  }

data Action
  = RequestPost
  | ReceivePost (Either String Post)
  | DeletePost
  | EditPost

init :: State
init = { post: Post { id: -1
                    , userId: -1
                    , title: ""
                    -- , body: ""
                    }
       , status: ""
       }

instance decodeJsonPost :: DecodeJson Post where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    userId <- obj .? "userId"
    title <- obj .? "title"
    -- body <- obj .? "body"
    pure $ Post { id, userId, title }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (ReceivePost (Left err)) state =
  noEffects $ state { status = "Error fetching post: " <> show err }
update (ReceivePost (Right post)) state =
  noEffects $ state { post = post, status = "Post" }
update (RequestPost) state =
  { state: state { status = "Fetching post..." }
  , effects : [ do
      res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
      let decode r = decodeJson r.response :: Either String Post
      let post = either (Left <<< show) decode res
      pure $ ReceivePost post
    ]
  }
update (DeletePost) state = noEffects $ state
update (EditPost) state = noEffects $ state

view :: State -> Html Action
view { status, post } =
  div []
    [ h1 [] [ text status ]
    , div []
      [ text $ "Post: " <> "ciao" ]
    ]
