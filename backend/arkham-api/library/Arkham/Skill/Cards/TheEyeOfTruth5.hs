module Arkham.Skill.Cards.TheEyeOfTruth5 (
  theEyeOfTruth5,
  theEyeOfTruth5Effect,
)
where

import Arkham.Card
import Arkham.Effect.Import
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTest)
import Arkham.Matcher
import Arkham.Name
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Treachery.Types (Field (..))

newtype TheEyeOfTruth5 = TheEyeOfTruth5 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEyeOfTruth5 :: SkillCard TheEyeOfTruth5
theEyeOfTruth5 = skill TheEyeOfTruth5 Cards.theEyeOfTruth5

instance RunMessage TheEyeOfTruth5 where
  runMessage msg s@(TheEyeOfTruth5 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      source <- fromJustNote "must be a skill test" <$> getSkillTestSource
      for_ source.treachery \tid -> skillTestResultOption "The Eye of Truth (5)" do
        addToVictory tid
        addToVictory attrs
        card <- field TreacheryCard tid
        createCardEffect Cards.theEyeOfTruth5 Nothing attrs (toCardId card)
      pure s
    _ -> TheEyeOfTruth5 <$> liftRunMessage msg attrs

newtype TheEyeOfTruth5Effect = TheEyeOfTruth5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEyeOfTruth5Effect :: EffectArgs -> TheEyeOfTruth5Effect
theEyeOfTruth5Effect = cardEffect TheEyeOfTruth5Effect Cards.theEyeOfTruth5

instance HasModifiersFor TheEyeOfTruth5Effect where
  getModifiersFor (TheEyeOfTruth5Effect a) = do
    withSkillTest \sid -> do
      maybeModified_ a sid do
        TreacherySource tid <- MaybeT getSkillTestSource
        CardIdTarget cardId <- pure a.target
        -- Because the effect is created during a skill test we need to make
        -- sure the treachery hasn't already been removed, such as this card's
        -- effect
        liftGuardM $ selectAny $ TreacheryWithId tid
        card <- lift $ toTitle <$> getCard cardId
        liftGuardM $ selectAny $ VictoryDisplayCardMatch $ basic $ CardWithId cardId
        treacheryCard <- lift $ fieldMap TreacheryCard toTitle tid
        guard $ card == treacheryCard
        pure [AddSkillIcons [#wild, #wild, #wild, #wild]]

instance RunMessage TheEyeOfTruth5Effect where
  runMessage msg e@(TheEyeOfTruth5Effect attrs) = runQueueT $ case msg of
    AddToVictory (TreacheryTarget tid) -> do
      -- If the card left the victory display and we are now adding it back, it
      -- won't be attached, so we should disable this
      cardId <- fieldMap TreacheryCard toCardId tid
      case attrs.target of
        CardIdTarget cardId' | cardId == cardId' -> disable attrs
        _ -> pure ()
      pure e
    _ -> TheEyeOfTruth5Effect <$> liftRunMessage msg attrs
