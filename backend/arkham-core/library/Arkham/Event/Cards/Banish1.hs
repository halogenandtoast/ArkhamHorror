module Arkham.Event.Cards.Banish1 (
  banish1,
  banish1Effect,
  Banish1 (..),
)
where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillTest.Base
import Arkham.SkillType

newtype Banish1 = Banish1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banish1 :: EventCard Banish1
banish1 =
  event Banish1 Cards.banish1

instance RunMessage Banish1 where
  runMessage msg e@(Banish1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ ChooseEvadeEnemy iid (toSource attrs) Nothing SkillWillpower NonEliteEnemy False
      pure e
    ChosenEvadeEnemy source eid | isSource attrs source -> do
      push $ createCardEffect Cards.banish1 Nothing attrs (EnemyTarget eid)
      pure e
    _ -> Banish1 <$> runMessage msg attrs

newtype Banish1Effect = Banish1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banish1Effect :: EffectArgs -> Banish1Effect
banish1Effect = cardEffect Banish1Effect Cards.banish1

instance RunMessage Banish1Effect where
  runMessage msg e@(Banish1Effect attrs@EffectAttrs {..}) = case msg of
    After (PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _) | source == effectSource -> do
      mSkillTestTarget <- getSkillTestTarget
      for_ mSkillTestTarget $ \case
        target@(EnemyTarget eid) | target == effectTarget -> do
          mSkillTest <- getSkillTest
          let
            modifierMsgs = case mSkillTest of
              Nothing -> []
              Just st ->
                let faces = map chaosTokenFace (skillTestRevealedChaosTokens st)
                in  [ createRoundModifier attrs eid [DoesNotReadyDuringUpkeep]
                    | any (`elem` faces) [Skull, Cultist, Tablet, ElderThing]
                    ]

          locations <- selectList (LocationWithoutModifier CannotBeEnteredByNonElite)
          let locationMsgs =
                if null locations
                  then []
                  else [chooseOrRunOne iid [targetLabel lid [EnemyMove eid lid] | lid <- locations]]

          pushAll $ locationMsgs <> modifierMsgs
        _ -> pure ()
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> Banish1Effect <$> runMessage msg attrs
