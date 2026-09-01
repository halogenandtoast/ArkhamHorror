module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonDrudge (newMoonDrudge) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype NewMoonDrudge = NewMoonDrudge EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonDrudge :: EnemyCard NewMoonDrudge
newMoonDrudge = enemy NewMoonDrudge Cards.newMoonDrudge

instance HasModifiersFor NewMoonDrudge where
  getModifiersFor (NewMoonDrudge a) = when a.ready do
    modifySelect
      a
      (InvestigatorAt $ locationWithEnemy a)
      [CannotTriggerAbilityMatching $ oneOf [AbilityOnEncounterCard, AbilityOnCard IsEncounterCard]]
