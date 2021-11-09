module Arkham.Types.Enemy.Cards.MadPatient
  ( madPatient
  , MadPatient(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Prey
import Arkham.Types.Timing qualified as Timing

newtype MadPatient = MadPatient EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madPatient :: EnemyCard MadPatient
madPatient = enemyWith
  MadPatient
  Cards.madPatient
  (2, Static 2, 3)
  (1, 0)
  ((preyL .~ MostRemainingSanity)
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

instance EnemyRunner env => RunMessage env MadPatient where
  runMessage msg (MadPatient attrs) = MadPatient <$> runMessage msg attrs
