module Arkham.Skill.Cards.Prescient (prescient, prescientEffect, Prescient (..)) where

import Arkham.ChaosToken
import Arkham.Effect.Import
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTest.Base
import Arkham.Trait (Trait (Spell))

newtype Prescient = Prescient SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prescient :: SkillCard Prescient
prescient = skill Prescient Cards.prescient

instance RunMessage Prescient where
  runMessage msg (Prescient attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == attrs.id -> do
      chooseOneM iid do
        labeled "Even" $ createCardEffect Cards.prescient (Just $ EffectInt 1) attrs iid
        labeled "Odd" $ createCardEffect Cards.prescient (Just $ EffectInt 2) attrs iid
        labeled "Symbol" $ createCardEffect Cards.prescient (Just $ EffectInt 3) attrs iid
      Prescient <$> liftRunMessage msg attrs
    _ -> Prescient <$> liftRunMessage msg attrs

newtype PrescientEffect = PrescientEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prescientEffect :: EffectArgs -> PrescientEffect
prescientEffect = cardEffect PrescientEffect Cards.prescient

instance RunMessage PrescientEffect where
  runMessage msg e@(PrescientEffect attrs) = runQueueT $ case msg of
    SkillTestEnds _ _ _ -> do
      mSkillTest <- getSkillTest
      let
        iid = case attrs.target of
          InvestigatorTarget iid' -> iid'
          _ -> error "invalid target, must be investigator"
        tokens = case mSkillTest of
          Just st -> map chaosTokenFace (skillTestRevealedChaosTokens st)
          Nothing -> error "no skill test"
        returnSpell = case attrs.meta of
          Just (EffectInt 1) -> any isEvenChaosToken tokens
          Just (EffectInt 2) -> any isOddChaosToken tokens
          Just (EffectInt 3) -> any isSymbolChaosToken tokens
          _ -> error "Invalid metadata"

      spells <-
        select
          $ InDiscardOf (InvestigatorWithId iid)
          <> BasicCardMatch
            (CardWithTrait Spell)
      disable attrs
      when (returnSpell && notNull spells) do
        focusCards spells \unfocus -> do
          chooseOneM iid do
            labeled "Do not return spell card" nothing
            targets spells $ addToHand iid . only
          push unfocus
      pure e
    _ -> PrescientEffect <$> liftRunMessage msg attrs
