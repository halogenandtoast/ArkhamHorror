module Arkham.Homebrew.AndThenThereWereNone.Enemies.RestlessSpirit_042 (
  restlessSpirit_042,
) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (Blank), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTest, skillTestMatches)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Placement (Placement (Limbo))

newtype RestlessSpirit_042 = RestlessSpirit_042 EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | "Spawn - Ethel Rogers' or Thomas Rogers' location."
restlessSpirit_042 :: EnemyCard RestlessSpirit_042
restlessSpirit_042 =
  enemy RestlessSpirit_042 Cards.restlessSpirit_042
    & setSpawnAt (guestsLocation [Assets.ethelRogers, Assets.thomasRogers])

{- | "While Restless Spirit is ready, treat the printed text box of each skill
committed to attacks or parley attempts against it as if they were blank (except
for /Traits/)."

Committing a skill moves it to 'Limbo', so that placement is what "each skill
committed" selects. 'Blank' is the engine's "text box blank (except for
Traits)": it leaves @cdSkills@ and @cdCardTraits@ alone, so the icons still
count, and 'Arkham.Game.applyBlank' strips every modifier the skill publishes.
-}
instance HasModifiersFor RestlessSpirit_042 where
  getModifiersFor (RestlessSpirit_042 a) =
    fromMaybe mempty <$> runMaybeT do
      guard a.ready
      st <- MaybeT getSkillTest
      liftGuardM $ skillTestMatches st.investigator (toSource a) st against
      lift $ modifySelect a (SkillWithPlacement Limbo) [Blank]
   where
    against =
      SkillTestOneOf [WhileAttackingAnEnemy (be a), WhileParleyingWithAnEnemy (be a)]
