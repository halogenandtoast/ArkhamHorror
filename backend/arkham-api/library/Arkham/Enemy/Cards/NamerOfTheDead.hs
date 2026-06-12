module Arkham.Enemy.Cards.NamerOfTheDead (namerOfTheDead) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.ReadOrDie.Helpers
import Arkham.Trait (Trait (Tome))

newtype NamerOfTheDead = NamerOfTheDead EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

namerOfTheDead :: EnemyCard NamerOfTheDead
namerOfTheDead =
  enemy NamerOfTheDead Cards.namerOfTheDead (2, Static 3, 2) (1, 1)
    & setOnlyPrey daisyWalker

instance HasAbilities NamerOfTheDead where
  getAbilities (NamerOfTheDead a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyWouldBeDefeated #when (be a)
      , scenarioI18n
          $ withI18nTooltip "namerOfTheDead.parley"
          $ skillTestAbility
          $ restricted
            a
            2
            ( youExist daisyWalker
                <> OnSameLocation
                <> AssetCount 4 (AssetControlledBy You <> AssetWithTrait Tome <> NonWeaknessAsset)
            )
            parleyAction_
      ]

instance RunMessage NamerOfTheDead where
  runMessage msg e@(NamerOfTheDead attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      exhaustThis attrs
      selectEach (locationIs Locations.orneLibrary) (enemyMoveTo (attrs.ability 1) attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      let tomes = CountAssets (AssetControlledBy (InvestigatorWithId iid) <> AssetWithTrait Tome)
      parley sid iid (attrs.ability 2) attrs #willpower
        $ MaxCalculation (Fixed 0)
        $ SubtractCalculation (Fixed 18) (MultiplyCalculation (Fixed 2) tomes)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      advanceCurrentAct attrs
      pure e
    _ -> NamerOfTheDead <$> liftRunMessage msg attrs
