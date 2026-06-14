module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfAmbition (sigilCarvedAlcoveStoryOfAmbition) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfAmbition = SigilCarvedAlcoveStoryOfAmbition LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfAmbition :: LocationCard SigilCarvedAlcoveStoryOfAmbition
sigilCarvedAlcoveStoryOfAmbition = location SigilCarvedAlcoveStoryOfAmbition Cards.sigilCarvedAlcoveStoryOfAmbition 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfAmbition where
  getAbilities (SigilCarvedAlcoveStoryOfAmbition a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage SigilCarvedAlcoveStoryOfAmbition where
  runMessage msg l@(SigilCarvedAlcoveStoryOfAmbition attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> randomDiscard iid (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 5)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      mClaw <- selectOne (assetIs Assets.obsidianClaw)
      for_ mClaw \claw -> do
        placeDoom (attrs.ability 2) claw 1
        investigators <- select (investigatorAt attrs)
        n <- getSpendableClueCount investigators
        x <- perPlayer 1
        when (n >= x) do
          lead <- getLead
          chooseOneM lead do
            labeled' "placeAdditionalDoomOnObsidianClaw" do
              spendCluesAsAGroup investigators x
              placeDoom (attrs.ability 2) claw 1
            labeled' "doNotPlaceAdditionalDoom" nothing
      pure l
    _ -> SigilCarvedAlcoveStoryOfAmbition <$> liftRunMessage msg attrs
