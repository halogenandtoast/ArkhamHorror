module Arkham.Stats (
  Stats (..),
  statsSkillValue,
) where

import Arkham.Prelude

import Arkham.SkillType

data Stats = Stats
  { health :: Int
  , sanity :: Int
  , willpower :: Int
  , intellect :: Int
  , combat :: Int
  , agility :: Int
  }
  deriving stock (Show)

statsSkillValue :: Stats -> SkillType -> Int
statsSkillValue Stats {..} = \case
  SkillWillpower -> willpower
  SkillIntellect -> intellect
  SkillCombat -> combat
  SkillAgility -> agility

instance Semigroup Stats where
  Stats a1 b1 c1 d1 e1 f1 <> Stats a2 b2 c2 d2 e2 f2 =
    Stats (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2) (f1 + f2)
instance Monoid Stats where
  mempty = Stats 0 0 0 0 0 0
