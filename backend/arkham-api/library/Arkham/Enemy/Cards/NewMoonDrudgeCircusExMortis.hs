module Arkham.Enemy.Cards.NewMoonDrudgeCircusExMortis (newMoonDrudgeCircusExMortis) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonDrudgeCircusExMortis = NewMoonDrudgeCircusExMortis EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonDrudgeCircusExMortis :: EnemyCard NewMoonDrudgeCircusExMortis
newMoonDrudgeCircusExMortis = enemy NewMoonDrudgeCircusExMortis Cards.newMoonDrudgeCircusExMortis

instance HasModifiersFor NewMoonDrudgeCircusExMortis where
  getModifiersFor (NewMoonDrudgeCircusExMortis a) = do
    modifySelf a [AddKeyword Keyword.Hunter]
    modifySelectWhen
      a
      a.ready
      (InvestigatorAt $ locationWithEnemy a)
      [CannotTriggerAbilityMatching AbilityOnEncounterCard]

instance RunMessage NewMoonDrudgeCircusExMortis where
  runMessage msg (NewMoonDrudgeCircusExMortis attrs) =
    NewMoonDrudgeCircusExMortis <$> runMessage msg attrs
