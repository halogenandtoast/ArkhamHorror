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
import Arkham.SkillType

newtype Metadata = Metadata {isFirst :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OneTwoPunch5 = OneTwoPunch5 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch5 :: EventCard OneTwoPunch5
oneTwoPunch5 = event (OneTwoPunch5 . (`with` Metadata True)) Cards.oneTwoPunch5

instance HasModifiersFor OneTwoPunch5 where
  getModifiersFor (InvestigatorTarget iid) (OneTwoPunch5 (a `With` metadata))
    | eventOwner a == iid && isFirst metadata =
        pure
          $ toModifiers a [SkillTestAutomaticallySucceeds]
  getModifiersFor _ _ = pure []

-- TODO: Also need to handle the repeat being tied to success in order to handle
-- the double or nothing effect

instance RunMessage OneTwoPunch5 where
  runMessage msg e@(OneTwoPunch5 (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier
            (toSource attrs)
            (InvestigatorTarget iid)
            (DamageDealt 1)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure e
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source && isFirst metadata -> do
          push $ CreateEffect "60132" Nothing source (InvestigatorTarget iid)
          pure . OneTwoPunch5 $ attrs `with` Metadata False
    _ -> OneTwoPunch5 . (`with` metadata) <$> runMessage msg attrs

newtype OneTwoPunch5Effect = OneTwoPunch5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch5Effect :: EffectArgs -> OneTwoPunch5Effect
oneTwoPunch5Effect = OneTwoPunch5Effect . uncurry4 (baseAttrs "60132")

instance RunMessage OneTwoPunch5Effect where
  runMessage msg (OneTwoPunch5Effect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ | not effectFinished -> do
      mSkillTest <- getSkillTest
      case (mSkillTest, effectTarget) of
        (Just skillTest, InvestigatorTarget iid) -> do
          case (skillTestResult skillTest, skillTestTarget skillTest) of
            (SucceededBy {}, EnemyTarget eid) -> do
              isStillAlive <- selectAny $ EnemyWithId eid
              player <- getPlayer iid
              push
                $ chooseOrRunOne player
                $ [ Label
                    "Fight that enemy again"
                    [ skillTestModifiers
                        (toSource attrs)
                        (InvestigatorTarget iid)
                        [SkillModifier SkillCombat 3, DamageDealt 2]
                    , ChooseFightEnemy
                        iid
                        (toSource attrs)
                        Nothing
                        SkillCombat
                        mempty
                        False
                    , DisableEffect (toId attrs)
                    ]
                  | isStillAlive
                  ]
                <> [ Label
                      "Do not fight that enemy again"
                      [DisableEffect (toId attrs)]
                   ]
            _ -> pure ()
        (_, _) -> error "invalid call"
      pure . OneTwoPunch5Effect $ attrs & finishedL .~ True
    _ -> OneTwoPunch5Effect <$> runMessage msg attrs
