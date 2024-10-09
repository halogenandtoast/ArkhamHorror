module Arkham.Matcher.Base where

class IsMatcher a
class IsMatcher b => Be a b | a -> b where
  be :: a -> b
