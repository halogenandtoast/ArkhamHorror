module Arkham.Act.Cards.BlazeOfGlory (blazeOfGlory) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SpreadingFlames.Helpers

newtype BlazeOfGlory = BlazeOfGlory ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blazeOfGlory :: ActCard BlazeOfGlory
blazeOfGlory = act (4, A) BlazeOfGlory Cards.blazeOfGlory Nothing

instance HasAbilities BlazeOfGlory where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ at_ YourLocation <> EnemyCanBeDamagedBySource (a.ability 1))
        $ freeTrigger (GroupClueCost (PerPlayer 1) Anywhere)
    , mkAbility a 2 $ ActionAbility #resign Nothing (ActionCost 1)
    , mkAbility a 3 $ Objective $ forced $ ifEnemyDefeated Enemies.servantOfFlameRagingFury
    ]

instance RunMessage BlazeOfGlory where
  runMessage msg a@(BlazeOfGlory attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select $ at_ (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource (attrs.ability 1)
      amount <- perPlayer 1
      chooseOrRunOneM iid $ targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) amount
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resign iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> BlazeOfGlory <$> liftRunMessage msg attrs
