module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfInfinity (sigilCarvedAlcoveStoryOfInfinity) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfInfinity = SigilCarvedAlcoveStoryOfInfinity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfInfinity :: LocationCard SigilCarvedAlcoveStoryOfInfinity
sigilCarvedAlcoveStoryOfInfinity = location SigilCarvedAlcoveStoryOfInfinity Cards.sigilCarvedAlcoveStoryOfInfinity 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfInfinity where
  getAbilities (SigilCarvedAlcoveStoryOfInfinity a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage SigilCarvedAlcoveStoryOfInfinity where
  runMessage msg l@(SigilCarvedAlcoveStoryOfInfinity attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- [Forced] - When Cthulhu enters this location: Each investigator takes 1 direct horror.
      eachInvestigator \iid -> directHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #intellect (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      -- On success, place 1 doom on the Tidal Tablet.
      selectOne (assetIs Assets.tidalTablet) >>= traverse_ \tablet -> do
        placeDoom (attrs.ability 2) tablet 1
        -- Investigators at this location may additionally spend 1 [per_investigator]
        -- clues, as a group, to place 1 more doom on it.
        chooseOneM iid do
          labeled' "placeAdditionalDoom"
            $ withCost iid (GroupClueCost (PerPlayer 1) (be attrs))
            $ placeDoom (attrs.ability 2) tablet 1
          labeled' "doNotPlaceAdditionalDoom" nothing
      pure l
    _ -> SigilCarvedAlcoveStoryOfInfinity <$> liftRunMessage msg attrs
