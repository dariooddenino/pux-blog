module App.Layout where

import App.Post as P
import App.PostList as PL
import App.NotFound as NotFound
import App.Routes
import Prelude (map, pure, ($), (#))
import Pux.Html (Html, div, h1, text)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, mapEffects, mapState)
import DOM (DOM)
import Pux.Router (link)


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
update (PostC action) state = P.update action state.post
                              # mapState (state { post = _ })
                              # mapEffects PostC
update (PostL action) state = PL.update action state.postList
                              # mapState (state { postList = _ })
                              # mapEffects PostL

routeEffects :: Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
routeEffects Home state = { state: state
                          , effects: [ pure PL.RequestPosts ] } # mapEffects PostL
routeEffects (Posts _ id) state = { state: state
                               , effects: [ pure (P.RequestPost id) ] } # mapEffects PostC
routeEffects _ state = noEffects $ state


view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ link "/" [] [ text "Pux Blog" ] ] 
    , case state.route of
        Home -> map PostL $ PL.view state.postList
        (Posts View id) -> map PostC $ P.view state.post
        (Posts Edit id) -> map PostC $ P.editView state.post
        AddPost -> map PostC $ P.view state.post
        NotFound -> NotFound.view state
    ]
