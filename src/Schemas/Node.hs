module Schemas.Node
  ( Node(..)
  , fromValue
  ) where

import Data.Aeson (Value(..))

import Schemas.Types


fromValue :: Value -> Node
fromValue (Object obj) = ObjectNode (fmap fromValue obj)
fromValue (Array arr) = ArrayNode (fmap fromValue arr)
fromValue (String str) = StringNode str
fromValue (Number num) = NumberNode num
fromValue (Bool bool) = BooleanNode bool
fromValue Null = NullNode
