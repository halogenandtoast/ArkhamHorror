module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfDefiance (sigilCarvedAlcoveStoryOfDefiance) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfDefiance = SigilCarvedAlcoveStoryOfDefiance LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfDefiance :: LocationCard SigilCarvedAlcoveStoryOfDefiance
sigilCarvedAlcoveStoryOfDefiance = location SigilCarvedAlcoveStoryOfDefiance Cards.sigilCarvedAlcoveStoryOfDefiance 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfDefiance where
  getAbilities (SigilCarvedAlcoveStoryOfDefiance a) =
    extendRevealed
      a
      [ -- [Forced] When Cthulhu enters this location: each investigator loses 2 resources.
        mkAbility a 1
          $ forced
          $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , -- [action] Test {agility} (5). On success place 1 doom on Grisly Mask.
        -- Investigators at this location may additionally spend 1 {perPlayer}
        -- clues, as a group, to place 1 more doom on it.
        scenarioI18n
          $ withI18nTooltip "sigilCarvedAlcoveStoryOfDefiance.test"
          $ skillTestAbility
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage SigilCarvedAlcoveStoryOfDefiance where
  runMessage msg l@(SigilCarvedAlcoveStoryOfDefiance attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> loseResources iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      selectOne (assetIs Assets.grislyMask) >>= traverse_ \grislyMaskAsset -> do
        placeDoom (attrs.ability 2) grislyMaskAsset 1
        -- Investigators here may additionally spend 1 clue per investigator, as a
        -- group, to place 1 more doom on the Grisly Mask.
        investigators <- select $ investigatorAt attrs
        n <- getSpendableClueCount investigators
        x <- perPlayer 1
        when (n >= x) do
          chooseOneM iid $ scenarioI18n do
            labeled' "placeAdditionalDoomOnGrislyMask" do
              spendCluesAsAGroup investigators x
              placeDoom (attrs.ability 2) grislyMaskAsset 1
            labeled' "doNotPlaceAdditionalDoom" nothing
      pure l
    _ -> SigilCarvedAlcoveStoryOfDefiance <$> liftRunMessage msg attrs
