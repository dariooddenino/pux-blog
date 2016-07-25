module App.Layout where

import App.Counter as Counter
import App.Post as Post
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound, Posts))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = Child (Counter.Action)
  | PostC (Post.Action)
  | PageView Route

type State =
  { route :: Route
  , post :: Post
  }

init :: State
init =
  { route: NotFound
  , post: Post.init
  }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = Counter.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> NotFound.view state
        (Posts id) -> map PostC $ Post.view state.post
        NotFound -> NotFound.view state
    ]
