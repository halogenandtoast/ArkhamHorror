{- | "Investigators at Restless Spirit's location lose all bonuses to skills
printed on assets (except bonuses added during actions)."

The clause is 'IgnoreSkillBonusesFromAssets' on every investigator here, read at
collection time in 'Arkham.Helpers.Modifiers.getModifiers''. An always-on asset
bonus is published onto the controller by the asset entity (Holy Rosary's
@controllerGets@), while a bonus an asset adds during an action arrives through a
window-modifier effect instead (Machete, .45 Automatic) -- so the card's own
parenthetical is the difference between those two, and the modifier draws the line
there rather than at the printed text. See its haddock for the exact reading of
"bonus to a skill".
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.RestlessSpirit_038 (
  restlessSpirit_038,
) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher

newtype RestlessSpirit_038 = RestlessSpirit_038 EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | "Spawn - Lawrence Wargrave's or William Blore's location."
restlessSpirit_038 :: EnemyCard RestlessSpirit_038
restlessSpirit_038 =
  enemy RestlessSpirit_038 Cards.restlessSpirit_038
    & setSpawnAt (guestsLocation [Assets.lawrenceWargrave, Assets.williamBlore])

instance HasModifiersFor RestlessSpirit_038 where
  getModifiersFor (RestlessSpirit_038 a) =
    unless (enemyDefeated a)
      $ modifySelect a (InvestigatorAt $ locationWithEnemy a) [IgnoreSkillBonusesFromAssets]
