module Arkham.Event.Cards.Banish1
  ( banish1
  , banish1Effect
  , Banish1(..)
  )
where

import Arkham.Prelude

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
import Arkham.Token

newtype Banish1 = Banish1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banish1 :: EventCard Banish1
banish1 =
  event Banish1 Cards.banish1

instance RunMessage Banish1 where
  runMessage msg e@(Banish1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ createCardEffect Cards.banish1 Nothing attrs iid
        , ChooseEvadeEnemy iid (toSource attrs) Nothing SkillWillpower NonEliteEnemy False
        ]
      pure e
    _ -> Banish1 <$> runMessage msg attrs

newtype Banish1Effect = Banish1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banish1Effect :: EffectArgs -> Banish1Effect
banish1Effect = cardEffect Banish1Effect Cards.banish1

instance RunMessage Banish1Effect where
  runMessage msg e@(Banish1Effect attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (EnemyTarget eid) _ _ -> do
      mSkillTest <- getSkillTest
      let
        modifierMsgs = case mSkillTest of
          Nothing -> []
          Just st ->
            let faces = map tokenFace (skillTestRevealedTokens st)
            in [ createRoundModifier attrs eid [DoesNotReadyDuringUpkeep]
               | any (`elem` faces) [Skull, Cultist, Tablet, ElderThing]
               ]

      locations <- selectList (LocationWithoutModifier CannotBeEnteredByNonElite)
      let locationMsgs = if null locations then [] else [chooseOrRunOne iid [targetLabel lid [EnemyMove eid lid] | lid <- locations]]

      pushAll $ locationMsgs <> modifierMsgs
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> Banish1Effect <$> runMessage msg attrs
