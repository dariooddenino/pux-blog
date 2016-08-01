module App.PostList where

import App.Post as P
import Control.Monad.Aff (attempt)
import DOM (DOM)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (snoc, filter)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get, post, delete)
import Prelude (bind, ($), (<>), show, (<<<), pure, map, const, (/=))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, text, ol, li, h2, button)
import Pux.Html.Attributes (key, className, type_)
import Pux.Html.Events (onClick)
import Pux.Router (link)

type Posts = Array P.Post

data Action = RequestPosts | ReceivePosts (Either String Posts) | CreatePost | DeletePost Int

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
update (CreatePost) state =
  { state: state { status = "Creating post" }
  , effects: [ do
      res <- attempt $ post "http://localhost:3001/api/posts" (encodeJson P.init.post)
      let decode r = decodeJson r.response :: Either String P.Post
      let newPost = either (Left <<< show) decode res
      pure $ ReceivePosts (map (snoc state.posts) newPost)
    ]
  }
update (DeletePost id) state =
  { state: state { status = "Deleting post" }
  , effects: [ do
      res <- attempt $ delete ("http://localhost:3001/api/posts/" <> show id)
      let decode r = decodeJson r.response :: Either String Boolean
      let response = either (Left <<< show) decode res
      case response of
        (Left err) -> pure $ ReceivePosts (Left err)
        (Right _) -> pure $ ReceivePosts $ Right (filter (\ (P.Post p) -> p.id /= Just id) state.posts)
   ]
  }

view :: State -> Html Action
view state =
  div []
    [ h1 [] [ text state.status ]
    , button [ type_ "button", onClick (const CreatePost) ] [ text "Create new post" ]
    , ol [] $ map postView state.posts
    ]

postView :: P.Post -> Html Action
postView (P.Post p) =
  case p.id of
    Nothing -> li [] []
    (Just id) -> li [ key (show id), className "post" ]
                 [ h2 []
                   [ link ("/posts/" <> show id) []
                     [ text p.title ]
                   ]
                 , button [ type_ "submit", onClick (const $ DeletePost id) ] [ text "Delete" ]
                 ]
