module Arkham.Location.Cards.LostAsteroid (lostAsteroid) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Helpers.Cost
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.SkillType

newtype LostAsteroid = LostAsteroid LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostAsteroid :: LocationCard LostAsteroid
lostAsteroid = location LostAsteroid Cards.lostAsteroid 0 (Static 0)

instance HasModifiersFor LostAsteroid where
  getModifiersFor (LostAsteroid a) = do
    phase <- getPhase
    when (phase == #mythos) do
      modifySelect a (investigatorAt a) [SkillModifier sType (-1) | sType <- allSkills]

instance HasAbilities LostAsteroid where
  getAbilities (LostAsteroid a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage LostAsteroid where
  runMessage msg l@(LostAsteroid attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      firstTaken <- selectAny $ LocationWithLabel "lostAsteroid1"
      setLocationLabel attrs.id $ if firstTaken then "lostAsteroid2" else "lostAsteroid1"
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      moveTo attrs iid attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      resources <- getSpendableResources iid
      when (resources >= 4) do
        chooseOneM iid do
          withI18n labeled' "skip" nothing
          scenarioI18n $ labeled' "lostAsteroid.pay" do
            spendResources iid 4
            skillTestModifier sid (attrs.ability 1) sid SkillTestAutomaticallySucceeds
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseSelectM iid (RevealedLocation <> not_ (be attrs)) $ moveTo (attrs.ability 1) iid
      toDiscardBy iid (attrs.ability 1) attrs
      pure l
    _ -> LostAsteroid <$> liftRunMessage msg attrs
