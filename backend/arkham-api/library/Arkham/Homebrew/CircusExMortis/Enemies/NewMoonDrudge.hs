module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonDrudge (newMoonDrudge) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonDrudge = NewMoonDrudge EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonDrudge :: EnemyCard NewMoonDrudge
newMoonDrudge = enemy NewMoonDrudge Cards.newMoonDrudge

instance HasModifiersFor NewMoonDrudge where
  getModifiersFor (NewMoonDrudge a) = do
    modifySelf a [AddKeyword Keyword.Hunter]
    modifySelectWhen
      a
      a.ready
      (InvestigatorAt $ locationWithEnemy a)
      -- "scenario cards" also covers encounter assets, whose source is not an
      -- encounter card source (Illusory Locus, Terrified Captives)
      [ CannotTriggerAbilityMatching
          $ AbilityOneOf [AbilityOnEncounterCard, AbilityOnCard IsEncounterCard]
      ]

instance RunMessage NewMoonDrudge where
  runMessage msg (NewMoonDrudge attrs) =
    NewMoonDrudge <$> runMessage msg attrs
