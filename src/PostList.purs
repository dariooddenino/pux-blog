module App.PostList where

import Prelude
import Data.Either (Either(..), either)
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, text, ol, li, h2, p)
import Pux.Html.Attributes (key, className)
import Network.HTTP.Affjax (AJAX, get)
import Control.Monad.Aff (attempt)
import Data.Argonaut (decodeJson)

import App.Post as P

type Posts = Array P.Post

data Action = RequestPosts | ReceivePosts (Either String Posts)

type State =
  { posts :: Posts
  , status :: String
  }

init :: State
init = { posts: []
       , status: "Fetching posts."
       }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (ReceivePosts (Left err)) state =
  noEffects $ state { status = "Error fetching posts: " <> show err }
update (ReceivePosts (Right posts)) state =
  noEffects $ state { posts = posts, status = "Posts" }
update (RequestPosts) state =
  { state: state { status = "Fetching posts..." }
  , effects: [ do
      res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
      let decode r = decodeJson r.response :: Either String Posts
      let posts = either (Left <<< show) decode res
      pure $ ReceivePosts posts
    ]
  }

view :: State -> Html Action
view state =
  div []
    [ h1 [] [ text state.status ]
    , ol [] $ map post state.posts
    ]

post :: P.Post -> Html Action
post (P.Post p) =
  li [ key (show p.id), className "post" ]
    [ h2 [] [ text p.title ]
    ]
