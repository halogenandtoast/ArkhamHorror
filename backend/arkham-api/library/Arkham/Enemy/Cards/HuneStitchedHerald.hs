module Arkham.Enemy.Cards.HuneStitchedHerald (huneStitchedHerald) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype HuneStitchedHerald = HuneStitchedHerald EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huneStitchedHerald :: EnemyCard HuneStitchedHerald
huneStitchedHerald = enemy HuneStitchedHerald Cards.huneStitchedHerald

instance HasAbilities HuneStitchedHerald where
  getAbilities (HuneStitchedHerald a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage HuneStitchedHerald where
  runMessage msg e@(HuneStitchedHerald attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      around <- getCluesAroundHubDimension
      scenarioI18n $ blueDecide iid do
        labeled' "placeDoomOnTheBlueAgenda" $ placeDoomOnFactionAgenda (attrs.ability 1) BlueFaction 1
        when (around > 0) do
          labeled' "removeCluesFromAroundHubDimension" $ removeCluesFromAroundHubDimension 2
        labeled' "huneStitchedHeraldAttacksYou" $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> HuneStitchedHerald <$> liftRunMessage msg attrs
