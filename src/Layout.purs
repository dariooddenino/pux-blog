module App.Layout where

import App.Post as P
import App.PostList as PL
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound, Posts))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import DOM (DOM)

data Action
  = PostC (P.Action)
  | PostL (PL.Action)
  | PageView Route

type State =
  { route :: Route
  , post :: P.State
  , postList :: PL.State
  }

init :: State
init =
  { route: NotFound
  , post: P.init
  , postList: PL.init
  }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
update (PageView route) state = noEffects $ state { route = route }
update (PostC action) state = let r = P.update action state.post in
  noEffects $ state { post = r.state }
update (PostL action) state = let r = PL.update action state.postList in
  noEffects $ state { postList = r.state }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Blog" ]
    , case state.route of
        Home -> map PostL $ PL.view state.postList
        (Posts id) -> map PostC $ P.view state.post
        NotFound -> NotFound.view state
    ]
