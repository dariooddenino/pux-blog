module App.Post where

import Control.Monad.Aff (attempt)
import DOM (DOM)
import Data.Argonaut (jsonEmptyObject, class DecodeJson, decodeJson, (.?), class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get, put)
import Prelude (bind, (<>), ($), pure, show, (<<<), const)
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, h2, p, text, input, button)
import Pux.Html.Attributes (type_, value)
import Pux.Html.Events (FormEvent, onClick, onInput)
import Pux.Router (link)

newtype Post = Post
 { id :: Maybe Int
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
  | EditPost Int
  | UpdateTitle FormEvent
  | UpdateBody FormEvent

init :: State
init = { post: Post { id: Nothing
                    , title: "New Post"
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

instance encodeJsonPost :: EncodeJson Post where
  encodeJson (Post post)
    = "id" := post.id
    ~> "title" := post.title
    ~> "body" := post.body
    ~> jsonEmptyObject

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
update (ReceivePost (Left err)) state =
  noEffects $ state { status = "Error fetching post: " <> show err }
update (ReceivePost (Right post)) state =
  noEffects $ state { post = post, status = "Post" }
update (RequestPost id) state =
  { state: state { status = "Fetching post " <> show id <> "..." }
  , effects: [ do
      res <- attempt $ get $ "http://localhost:3001/api/posts/" <> show id
      let decode r = decodeJson r.response :: Either String Post
      let post = either (Left <<< show) decode res
      pure $ ReceivePost post
    ]
  }
update (DeletePost) state = noEffects $ state
update (EditPost id) state@{ status, post } =
  { state: state { status = "Saving post..." }
  , effects: [ do
      res <- attempt $ put ("http://localhost:3001/api/posts/" <> show id) (encodeJson post)
      let decode r = decodeJson r.response :: Either String Post
      let post = either (Left <<< show) decode res
      pure $ ReceivePost post
    ]
  }
update (UpdateTitle ev) { status: status, post: (Post post) } =
  let newPost = Post $ post { title = ev.target.value } in
  noEffects $ { status: status, post: newPost }
update (UpdateBody ev) { status: status, post: (Post post) } =
  let newPost = Post $ post { body = ev.target.value } in
  noEffects $ { status: status, post: newPost }

view :: State -> Html Action
view { status: status, post: (Post post) } =
      case post.id of
        Nothing -> div [] []
        (Just id) ->
          div []
          [ h1 [] [ text status ]
          , link ("/posts/" <> show id <> "/edit") [] [ text "Edit" ]
          , div []
            [ h2 [] [ text post.title ]
            , p [] [ text post.body ]
            ]
          ]

editView :: State -> Html Action
editView { status: status, post: (Post post) } =
  case post.id of
    Nothing -> div [] []
    (Just id) ->
      div []
      [ h1 [] [ text status ]
      , div []
        [ input [ type_ "text", value post.title, onInput UpdateTitle ] []
        , input [ type_ "textarea", value post.body, onInput UpdateBody ] []
        , link ("/posts/" <> show id) [] [text "Cancel"]
        , button [ type_ "submit", onClick (const $ EditPost id) ] [ text "Save" ]
        ]
      ]
