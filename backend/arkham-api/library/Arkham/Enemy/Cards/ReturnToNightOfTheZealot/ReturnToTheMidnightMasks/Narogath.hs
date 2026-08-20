module Arkham.Enemy.Cards.ReturnToNightOfTheZealot.ReturnToTheMidnightMasks.Narogath (narogath) where

import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.CardDefs.ReturnToNightOfTheZealot.ReturnToTheMidnightMasks qualified as Cards
import Arkham.Enemy.Runner
import Arkham.ForMovement
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait
import Arkham.Trait qualified as Trait

newtype Narogath = Narogath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

narogath :: EnemyCard Narogath
narogath =
  enemyWith Narogath Cards.narogath
    $ preyL
    .~ Prey (NearestToEnemy $ withTrait Trait.Cultist <> not_ (enemyIs Cards.narogath))

instance HasModifiersFor Narogath where
  getModifiersFor (Narogath a) = do
    modifySelectWhen
      a
      a.ready
      (InvestigatorAt $ AccessibleFrom NotForMovement $ locationWithEnemy a)
      [CannotPerformAction $ EnemyAction Parley $ EnemyWithTrait Cultist]
    n <- perPlayer 3
    modifySelf a [HealthModifier n]

instance RunMessage Narogath where
  runMessage msg (Narogath attrs) = Narogath <$> runMessage msg attrs
