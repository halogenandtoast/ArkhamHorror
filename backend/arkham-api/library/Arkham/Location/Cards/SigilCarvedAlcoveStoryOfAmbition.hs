module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfAmbition (sigilCarvedAlcoveStoryOfAmbition) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfAmbition = SigilCarvedAlcoveStoryOfAmbition LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfAmbition :: LocationCard SigilCarvedAlcoveStoryOfAmbition
sigilCarvedAlcoveStoryOfAmbition =
  location SigilCarvedAlcoveStoryOfAmbition Cards.sigilCarvedAlcoveStoryOfAmbition 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfAmbition where
  getAbilities (SigilCarvedAlcoveStoryOfAmbition a) =
    extendRevealed
      a
      [ -- [Forced] When Cthulhu enters this location: each investigator discards 1
        -- card at random from their hand.
        mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , -- [action]: Test {willpower} (5). On success, place 1 doom on the Obsidian
        -- Claw, with the option to spend clues as a group for 1 more.
        scenarioI18n
          $ withI18nTooltip "sigilCarvedAlcoveStoryOfAmbition.test"
          $ skillTestAbility
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage SigilCarvedAlcoveStoryOfAmbition where
  runMessage msg l@(SigilCarvedAlcoveStoryOfAmbition attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> randomDiscard iid (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      loadArtifact (attrs.ability 2) attrs iid Assets.obsidianClaw "placeAdditionalDoomOnObsidianClaw"
      pure l
    _ -> SigilCarvedAlcoveStoryOfAmbition <$> liftRunMessage msg attrs
