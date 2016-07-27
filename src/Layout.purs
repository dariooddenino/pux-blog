module App.Layout where

import App.Post as P
import App.PostList as PL
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound, Posts))
import Prelude
import Pux.Html (Html, div, h1, text)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, mapEffects, mapState)
import DOM (DOM)
-- import Data.Argonaut (decodeJson)
-- import Control.Monad.Aff (attempt)


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
update (PageView route) state = routeEffects route (state { route = route })
update (PostC action) state = let r = P.update action state.post in
  noEffects $ state { post = r.state }
update (PostL action) state = PL.update action state.postList
                              # mapState (state { postList = _ })
                              # mapEffects PostL

routeEffects :: Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
routeEffects Home state = { state: state
                          , effects: [ pure PL.RequestPosts ] } # mapEffects PostL
routeEffects _ state = noEffects $ state


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
