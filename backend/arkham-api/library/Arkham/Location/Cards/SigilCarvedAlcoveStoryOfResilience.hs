module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfResilience (sigilCarvedAlcoveStoryOfResilience) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfResilience = SigilCarvedAlcoveStoryOfResilience LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfResilience :: LocationCard SigilCarvedAlcoveStoryOfResilience
sigilCarvedAlcoveStoryOfResilience = location SigilCarvedAlcoveStoryOfResilience Cards.sigilCarvedAlcoveStoryOfResilience 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfResilience where
  getAbilities (SigilCarvedAlcoveStoryOfResilience a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage SigilCarvedAlcoveStoryOfResilience where
  runMessage msg l@(SigilCarvedAlcoveStoryOfResilience attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> directDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      selectOne (assetIs Assets.barrierNode) >>= traverse_ \node -> do
        placeDoom (attrs.ability 2) node 1
        -- "Investigators at this location may additionally spend 1 clue per
        -- investigator, as a group, to place 1 more doom on the Barrier Node."
        chooseOneM iid do
          labeled' "placeAdditionalDoomOnBarrierNode"
            $ withCost iid (GroupClueCost (PerPlayer 1) (be attrs))
            $ placeDoom (attrs.ability 2) node 1
          labeled' "doNotPlaceAdditionalDoom" nothing
      pure l
    _ -> SigilCarvedAlcoveStoryOfResilience <$> liftRunMessage msg attrs
