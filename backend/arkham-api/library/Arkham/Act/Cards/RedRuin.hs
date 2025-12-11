module Arkham.Act.Cards.RedRuin (redRuin) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype RedRuin = RedRuin ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

redRuin :: ActCard RedRuin
redRuin = act (2, A) RedRuin Cards.redRuin Nothing

instance HasAbilities RedRuin where
  getAbilities = actAbilities \a ->
    [ doesNotProvokeAttacksOfOpportunityFrom (enemyIs Enemies.mimeticNemesisOtherworldlySubjugator)
        $ restricted a 1 (exists $ enemyIs Enemies.mimeticNemesisOtherworldlySubjugator <> at_ YourLocation)
        $ actionAbilityWithCost (ClueCost $ Static 1)
    , mkAbility a 2
        $ Objective
        $ forced
        $ AssetEntersPlay #when
        $ assetIs Assets.theRedGlovedManHeWasAlwaysThere
    ]

instance RunMessage RedRuin where
  runMessage msg a@(RedRuin attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mimeticNemesis <- selectJust $ enemyIs Enemies.mimeticNemesisOtherworldlySubjugator
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2 mimeticNemesis
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> RedRuin <$> liftRunMessage msg attrs
