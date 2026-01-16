module Arkham.Event.Events.OneTwoPunch (oneTwoPunch) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillTest.Base

newtype OneTwoPunch = OneTwoPunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch :: EventCard OneTwoPunch
oneTwoPunch = event OneTwoPunch Cards.oneTwoPunch

instance RunMessage OneTwoPunch where
  runMessage msg e@(OneTwoPunch attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #combat 1)
      chooseFightEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) | getEventMetaDefault True attrs -> do
      skillTest <- fromJustNote "invalid call" <$> getSkillTest
      isStillAlive <- case skillTest.target of
        EnemyTarget eid -> selectAny $ EnemyWithId eid
        LocationTarget lid -> selectAny $ LocationWithId lid
        AssetTarget aid -> selectAny $ AssetWithId aid
        _ -> error "invalid call"
      sid <- getRandom
      enabled <- capture $ skillTestModifiers sid attrs iid [SkillModifier #combat 2, DamageDealt 1]
      chooseOrRunOneM iid $ cardI18n do
        labeledValidate' isStillAlive "oneTwoPunch.again" do
          push $ BeginSkillTestWithPreMessages' enabled (resetSkillTest sid skillTest)
        labeled' "oneTwoPunch.skip" nothing
      pure . OneTwoPunch $ attrs & setMeta False
    _ -> OneTwoPunch <$> liftRunMessage msg attrs
