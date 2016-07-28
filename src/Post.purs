module App.Post where

import Prelude
import Data.Either (Either(..), either)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, h2, p, text)
import Network.HTTP.Affjax (AJAX, get, post, put)
import Control.Monad.Aff (attempt)
import DOM (DOM)

newtype Post = Post
 { id :: Int
 , title :: String
 , body :: String
 }

type State =
  { post :: Post
  , status :: String
  }

data Action
  = RequestPost Int
  | ReceivePost (Either String Post)
  | DeletePost
  | EditPost

init :: State
init = { post: Post { id: -1
                    , title: ""
                    , body: ""
                    }
       , status: ""
       }

instance decodeJsonPost :: DecodeJson Post where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    body <- obj .? "body"
    pure $ Post { id, title, body }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
update (ReceivePost (Left err)) state =
  noEffects $ state { status = "Error fetching post: " <> show err }
update (ReceivePost (Right post)) state =
  noEffects $ state { post = post, status = "Post" }
update (RequestPost id) state =
  { state: state { status = "Fetching post " <> show id <> "..." }
  , effects : [ do
      res <- attempt $ get $ "http://localhost:3001/api/posts/" <> show id
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
    , postView post
    ]

postView :: Post -> Html Action
postView (Post post) =
  div []
  [ h2 [] [ text post.title ]
  , p [] [ text post.body ]
  ]
