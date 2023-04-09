module Arkham.Skill.Cards.Prescient
  ( prescient
  , prescientEffect
  , Prescient(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillTest.Base
import Arkham.Token
import Arkham.Trait ( Trait (Spell) )

newtype Prescient = Prescient SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prescient :: SkillCard Prescient
prescient = skill Prescient Cards.prescient

instance RunMessage Prescient where
  runMessage msg s@(Prescient attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      push $ chooseOne
        iid
        [ Label
          "Even"
          [createCardEffect Cards.prescient (Just $ EffectInt 1) attrs iid]
        , Label
          "Odd"
          [createCardEffect Cards.prescient (Just $ EffectInt 2) attrs iid]
        , Label
          "Symbol"
          [createCardEffect Cards.prescient (Just $ EffectInt 3) attrs iid]
        ]
      pure s
    _ -> Prescient <$> runMessage msg attrs

newtype PrescientEffect = PrescientEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prescientEffect :: EffectArgs -> PrescientEffect
prescientEffect = cardEffect PrescientEffect Cards.prescient

instance RunMessage PrescientEffect where
  runMessage msg e@(PrescientEffect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ -> do
      mSkillTest <- getSkillTest
      let
        iid = case effectTarget of
          InvestigatorTarget iid' -> iid'
          _ -> error "invalid target, must be investigator"
        tokens = case mSkillTest of
          Just st -> map tokenFace (skillTestRevealedTokens st)
          Nothing -> error "no skill test"
        returnSpell = case effectMetadata of
          Just (EffectInt 1) -> any isEvenToken tokens
          Just (EffectInt 2) -> any isOddToken tokens
          Just (EffectInt 3) -> any isSymbolToken tokens
          _ -> error "Invalid metadata"

      spells <-
        selectList $ InDiscardOf (InvestigatorWithId iid) <> BasicCardMatch
          (CardWithTrait Spell)
      pushAll $ DisableEffect effectId : if returnSpell && notNull spells
        then
          [ FocusCards spells
          , chooseOne iid
          $ Label "Do not return spell card" []
          : [ targetLabel (toCardId spell) [addToHand iid spell]
            | spell <- spells
            ]
          , UnfocusCards
          ]
        else []
      pure e
    _ -> PrescientEffect <$> runMessage msg attrs
