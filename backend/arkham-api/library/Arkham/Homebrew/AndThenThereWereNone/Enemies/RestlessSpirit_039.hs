module Arkham.Homebrew.AndThenThereWereNone.Enemies.RestlessSpirit_039 (
  restlessSpirit_039,
) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest, skillTestMatches)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Firearm, Ranged, Spell))

newtype RestlessSpirit_039 = RestlessSpirit_039 EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | "Spawn - John MacArthur's or Philip Lombard's location."
restlessSpirit_039 :: EnemyCard RestlessSpirit_039
restlessSpirit_039 =
  enemy RestlessSpirit_039 Cards.restlessSpirit_039
    & setSpawnAt (guestsLocation [Assets.johnMacArthur, Assets.philipLombard])

{- | "Restless Spirit gets -1 fight and loses retaliate during attacks on
/Firearm/, /Ranged/, or /Spell/ cards."

Constant text with no printed ability, so it is a 'HasModifiersFor' scoped to
the live skill test. The fight value is read through
@EnemyMaybeFieldCalculation ... EnemyFight@ while the test runs, and retaliate
through 'getModifiedKeywords' from the @Failed Fight@ handler, which the test
also outlives -- so both reach their check.

The basic fight action is sourced on the enemy itself, so only an attack made
with a card answers 'SkillTestSourceMatches' here, which is what "attacks on
... cards" means.
-}
instance HasModifiersFor RestlessSpirit_039 where
  getModifiersFor (RestlessSpirit_039 a) =
    fromMaybe mempty <$> runMaybeT do
      st <- MaybeT getSkillTest
      liftGuardM $ skillTestMatches st.investigator (toSource a) st warded
      lift $ modifySelf a [EnemyFight (-1), RemoveKeyword Keyword.Retaliate]
   where
    warded =
      WhileAttackingAnEnemy (be a)
        <> SkillTestSourceMatches
          (oneOf [SourceWithTrait Firearm, SourceWithTrait Ranged, SourceWithTrait Spell])
