module App.PostList where

import Prelude (bind, ($), (<>), show, (<<<), pure, map)
import Data.Either (Either(..), either)
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, text, ol, li, h2)
import Pux.Html.Attributes (key, className)
import Pux.Router (link)
import Network.HTTP.Affjax (AJAX, get)
import Control.Monad.Aff (attempt)
import Data.Argonaut (decodeJson)
import DOM (DOM)
import Data.Maybe (Maybe(..))

import App.Post as P

type Posts = Array P.Post

data Action = RequestPosts | ReceivePosts (Either String Posts)

type State =
  { posts :: Posts
  , status :: String
  }

init :: State
init = { posts: []
       , status: "..."
       }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
update (ReceivePosts (Left err)) state =
  noEffects $ state { status = "Error fetching posts: " <> show err }
update (ReceivePosts (Right posts)) state =
  noEffects $ state { posts = posts, status = "Posts" }
update (RequestPosts) state =
  { state: state { status = "Fetching posts..." }
  , effects: [ do
      res <- attempt $ get "http://localhost:3001/api/posts"
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
  case p.id of
    Nothing -> li [] [ text "Error" ]
    (Just id) -> li [ key (show id), className "post" ]
                 [ h2 []
                   [ link ("/posts/" <> show id) []
                     [ text p.title ]
                   ]
                 ]
