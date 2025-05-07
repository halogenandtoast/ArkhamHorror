module Arkham.Matcher.Base where

import Arkham.Prelude

class IsMatcher a
class IsMatcher b => Be a b where
  be :: a -> b

class IsMatcher matcher => Has matcher a where
  has :: a -> matcher

class FirstOf a where
  firstOf :: [a] -> a

class OneOf a where
  oneOf :: [a] -> a

mapOneOf :: (OneOf b, MonoFoldable t, Element t ~ a) => (a -> b) -> t -> b
mapOneOf f ts = case otoList ts of
  [x] -> f x
  xs -> oneOf $ map f xs

beOneOf :: (Be a b, OneOf b) => [a] -> b
beOneOf = mapOneOf be

notOneOf :: (Not a, OneOf a) => [a] -> a
notOneOf = not_ . oneOf
