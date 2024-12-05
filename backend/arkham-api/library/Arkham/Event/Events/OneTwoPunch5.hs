module Arkham.Event.Events.OneTwoPunch5 (oneTwoPunch5, OneTwoPunch5 (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Fight
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest.Base

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
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ a (SkillTestTarget st.id) do
        guard $ isFirst meta
        iid <- MaybeT getSkillTestInvestigator
        guard $ a.owner == iid
        pure [SkillTestAutomaticallySucceeds]

instance RunMessage OneTwoPunch5 where
  runMessage msg e@(OneTwoPunch5 (attrs `With` metadata)) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid attrs
      enabled <- skillTestModifier sid attrs iid (DamageDealt 1)
      pushAll [enabled, chooseFight]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) | isFirst metadata -> do
      skillTest <- fromJustNote "invalid call" <$> getSkillTest
      case skillTestTarget skillTest of
        EnemyTarget eid -> do
          isStillAlive <- selectAny $ EnemyWithId eid
          player <- getPlayer iid
          sid <- getRandom
          enabled <- skillTestModifiers sid attrs iid [SkillModifier #combat 3, DamageDealt 2]
          push
            $ chooseOrRunOne player
            $ [ Label
                "Fight that enemy again"
                [BeginSkillTestWithPreMessages' [enabled] (resetSkillTest sid skillTest)]
              | isStillAlive
              ]
            <> [Label "Do not fight that enemy again" []]
        _ -> error "invalid call"
      pure . OneTwoPunch5 $ attrs `with` Metadata False
    _ -> OneTwoPunch5 . (`with` metadata) <$> runMessage msg attrs
