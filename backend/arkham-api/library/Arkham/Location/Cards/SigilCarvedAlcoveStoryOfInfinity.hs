module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfInfinity (sigilCarvedAlcoveStoryOfInfinity) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfInfinity = SigilCarvedAlcoveStoryOfInfinity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfInfinity :: LocationCard SigilCarvedAlcoveStoryOfInfinity
sigilCarvedAlcoveStoryOfInfinity =
  location SigilCarvedAlcoveStoryOfInfinity Cards.sigilCarvedAlcoveStoryOfInfinity 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfInfinity where
  getAbilities (SigilCarvedAlcoveStoryOfInfinity a) =
    extendRevealed
      a
      [ -- [Forced] When Cthulhu enters this location: each investigator takes 1
        -- direct horror.
        mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , -- [action]: Test {intellect} (5). On success, place 1 doom on the Tidal
        -- Tablet, with the option to spend clues as a group for 1 more.
        scenarioI18n
          $ withI18nTooltip "sigilCarvedAlcoveStoryOfInfinity.test"
          $ skillTestAbility
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage SigilCarvedAlcoveStoryOfInfinity where
  runMessage msg l@(SigilCarvedAlcoveStoryOfInfinity attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> directHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #intellect (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      loadArtifact (attrs.ability 2) attrs iid Assets.tidalTablet "placeAdditionalDoomOnTidalTablet"
      pure l
    _ -> SigilCarvedAlcoveStoryOfInfinity <$> liftRunMessage msg attrs
