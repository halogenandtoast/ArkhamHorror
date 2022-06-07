module Arkham.Enemy.Cards.MadPatient
  ( madPatient
  , MadPatient(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype MadPatient = MadPatient EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madPatient :: EnemyCard MadPatient
madPatient = enemyWith
  MadPatient
  Cards.madPatient
  (2, Static 2, 3)
  (1, 0)
  ((preyL .~ Prey MostRemainingSanity)
  . (spawnAtL ?~ NearestLocationToYou (LocationWithTitle "Asylum Halls"))
  )

instance HasAbilities MadPatient where
  getAbilities (MadPatient a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyAttacked Timing.When You AnySource
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage MadPatient where
  runMessage msg (MadPatient attrs) = MadPatient <$> runMessage msg attrs
