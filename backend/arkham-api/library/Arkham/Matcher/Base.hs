module Arkham.Matcher.Base where

import Arkham.Prelude

class IsMatcher a
class IsMatcher b => Be a b where
  be :: a -> b

class IsMatcher matcher => Has matcher a where
  has :: a -> matcher

class OneOf a where
  oneOf :: [a] -> a

mapOneOf :: OneOf b => (a -> b) -> [a] -> b
mapOneOf f = oneOf . map f

beOneOf :: (Be a b, OneOf b) => [a] -> b
beOneOf = mapOneOf be

notOneOf :: (Not a, OneOf a) => [a] -> a
notOneOf = not_ . oneOf
