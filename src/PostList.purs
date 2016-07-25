module App.PostList where

import Data.Either (Either(..))

import App.Post (Post)


type Posts = Array Post

data Action = RequestPosts | ReceivePosts (Either String Posts)
