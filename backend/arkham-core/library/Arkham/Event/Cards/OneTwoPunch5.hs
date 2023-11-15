module Arkham.Event.Cards.OneTwoPunch5 (
  oneTwoPunch5,
  OneTwoPunch5 (..),
  oneTwoPunch5Effect,
  OneTwoPunch5Effect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.SkillTestResult

newtype Metadata = Metadata {isFirst :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OneTwoPunch5 = OneTwoPunch5 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch5 :: EventCard OneTwoPunch5
oneTwoPunch5 = event (OneTwoPunch5 . (`with` Metadata True)) Cards.oneTwoPunch5

instance HasModifiersFor OneTwoPunch5 where
  getModifiersFor (InvestigatorTarget iid) (OneTwoPunch5 (a `With` meta)) | a.owner == iid && isFirst meta = do
    pure $ toModifiers a [SkillTestAutomaticallySucceeds]
  getModifiersFor _ _ = pure []

-- TODO: Also need to handle the repeat being tied to success in order to handle
-- the double or nothing effect

instance RunMessage OneTwoPunch5 where
  runMessage msg e@(OneTwoPunch5 (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs iid (DamageDealt 1)
        , chooseFightEnemy iid attrs #combat
        ]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) | isFirst metadata -> do
      push $ createCardEffect Cards.oneTwoPunch5 Nothing attrs iid
      pure . OneTwoPunch5 $ attrs `with` Metadata False
    _ -> OneTwoPunch5 . (`with` metadata) <$> runMessage msg attrs

newtype OneTwoPunch5Effect = OneTwoPunch5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch5Effect :: EffectArgs -> OneTwoPunch5Effect
oneTwoPunch5Effect = cardEffect OneTwoPunch5Effect Cards.oneTwoPunch5

instance HasModifiersFor OneTwoPunch5Effect where
  getModifiersFor target (OneTwoPunch5Effect attrs) | attrs.target == target && attrs.finished = do
    pure $ toModifiers attrs [SkillModifier #combat 3, DamageDealt 2]
  getModifiersFor _ _ = pure []

instance RunMessage OneTwoPunch5Effect where
  runMessage msg e@(OneTwoPunch5Effect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ | attrs.finished -> do
      push $ disable attrs
      pure e
    SkillTestEnds _ _ | not attrs.finished -> do
      mSkillTest <- getSkillTest
      case (mSkillTest, effectTarget) of
        (Just skillTest, InvestigatorTarget iid) -> do
          case (skillTestResult skillTest, skillTestTarget skillTest) of
            (SucceededBy {}, EnemyTarget eid) -> do
              isStillAlive <- selectAny $ EnemyWithId eid
              player <- getPlayer iid
              push
                $ chooseOrRunOne player
                $ [ Label "Fight that enemy again" [FightEnemy iid eid attrs.source Nothing #combat False]
                  | isStillAlive
                  ]
                <> [Label "Do not fight that enemy again" [disable attrs]]
            _ -> pure ()
        (_, _) -> error "invalid call"
      pure . OneTwoPunch5Effect $ attrs & finishedL .~ True
    _ -> OneTwoPunch5Effect <$> runMessage msg attrs
