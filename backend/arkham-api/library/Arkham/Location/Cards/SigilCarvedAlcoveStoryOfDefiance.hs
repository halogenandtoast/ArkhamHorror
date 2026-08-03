module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfDefiance (sigilCarvedAlcoveStoryOfDefiance) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfDefiance = SigilCarvedAlcoveStoryOfDefiance LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfDefiance :: LocationCard SigilCarvedAlcoveStoryOfDefiance
sigilCarvedAlcoveStoryOfDefiance =
  location SigilCarvedAlcoveStoryOfDefiance Cards.sigilCarvedAlcoveStoryOfDefiance 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfDefiance where
  getAbilities (SigilCarvedAlcoveStoryOfDefiance a) =
    extendRevealed
      a
      [ -- [Forced] When Cthulhu enters this location: each investigator loses 2
        -- resources.
        mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , -- [action]: Test {agility} (5). On success, place 1 doom on the Grisly
        -- "Mask", with the option to spend clues as a group for 1 more.
        scenarioI18n
          $ withI18nTooltip "sigilCarvedAlcoveStoryOfDefiance.test"
          $ skillTestAbility
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage SigilCarvedAlcoveStoryOfDefiance where
  runMessage msg l@(SigilCarvedAlcoveStoryOfDefiance attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> loseResources iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      loadArtifact (attrs.ability 2) attrs iid Assets.grislyMask "placeAdditionalDoomOnGrislyMask"
      pure l
    _ -> SigilCarvedAlcoveStoryOfDefiance <$> liftRunMessage msg attrs
