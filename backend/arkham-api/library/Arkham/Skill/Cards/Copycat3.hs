module Arkham.Skill.Cards.Copycat3 (copycat3, copycat3Effect, Copycat3 (..)) where

import Arkham.Deck qualified as Deck
import Arkham.Effect.Import
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Copycat3 = Copycat3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

copycat3 :: SkillCard Copycat3
copycat3 = skill Copycat3 Cards.copycat3

instance RunMessage Copycat3 where
  runMessage msg (Copycat3 attrs) = runQueueT case msg of
    InvestigatorCommittedSkill iid sid | sid == attrs.id -> do
      iids <- select $ not_ $ InvestigatorWithId iid
      iidsWithCommittableCards <- forMaybeM iids $ \iid' -> do
        committableCards <- select $ CommittableCard (InvestigatorWithId iid) $ inDiscardOf iid' <> #skill
        pure $ guard (notNull committableCards) $> (iid', committableCards)
      unless (null iidsWithCommittableCards) do
        focusCards (concatMap snd iidsWithCommittableCards) \unfocus -> do
          chooseOneM iid do
            for_ iidsWithCommittableCards \(iid', cards) -> do
              targets cards \card -> do
                push $ CommitCard iid card
                createCardEffect Cards.copycat3 (Just $ EffectMetaTarget (toTarget card)) attrs iid'
                push unfocus
      Copycat3 <$> liftRunMessage msg attrs
    _ -> Copycat3 <$> liftRunMessage msg attrs

newtype Copycat3Effect = Copycat3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

copycat3Effect :: EffectArgs -> Copycat3Effect
copycat3Effect = cardEffect Copycat3Effect Cards.copycat3

instance RunMessage Copycat3Effect where
  runMessage msg e@(Copycat3Effect attrs) = runQueueT $ case msg of
    SkillTestEnds _ _ _ -> do
      case (attrs.meta, attrs.target) of
        (Just (EffectMetaTarget (CardIdTarget cardId)), InvestigatorTarget iid) -> do
          card <- getCard cardId
          disable attrs
          push $ PutCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid) card
        _ -> error "invalid target or effectMetaTarget"
      pure e
    _ -> Copycat3Effect <$> liftRunMessage msg attrs
