module Arkham.Event.Events.OneTwoPunch5 (oneTwoPunch5) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Action
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator)
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype Metadata = Metadata {isFirst :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OneTwoPunch5 = OneTwoPunch5 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch5 :: EventCard OneTwoPunch5
oneTwoPunch5 = event (OneTwoPunch5 . (`with` Metadata True)) Cards.oneTwoPunch5

instance HasModifiersFor OneTwoPunch5 where
  getModifiersFor (OneTwoPunch5 (a `With` meta)) =
    getSkillTest >>= traverse_ \st ->
      maybeModified_ a st do
        guard $ isFirst meta
        iid <- MaybeT getSkillTestInvestigator
        guard $ a.owner == iid
        pure [SkillTestAutomaticallySucceeds]

instance RunMessage OneTwoPunch5 where
  runMessage msg e@(OneTwoPunch5 (attrs `With` metadata)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (DamageDealt 1)
      chooseFightEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) | isFirst metadata -> do
      fightable <- hasFightActions iid attrs (Arkham.Matcher.DuringTurn You) (Window.defaultWindows iid)
      when fightable do
        sid <- getRandom
        chooseOneM iid do
          labeled "Fight again" do
            skillTestModifiers sid attrs iid [SkillModifier #combat 3, DamageDealt 2]
            chooseFightEnemy sid iid attrs
          labeled "Do not fight again" nothing
      pure . OneTwoPunch5 $ attrs `with` Metadata False
    _ -> OneTwoPunch5 . (`with` metadata) <$> liftRunMessage msg attrs
