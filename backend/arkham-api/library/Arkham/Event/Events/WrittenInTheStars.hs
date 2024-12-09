module Arkham.Event.Events.WrittenInTheStars (
  writtenInTheStars,
  writtenInTheStarsEffect,
  WrittenInTheStars (..),
)
where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getIsCommittable)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype WrittenInTheStars = WrittenInTheStars EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writtenInTheStars :: EventCard WrittenInTheStars
writtenInTheStars = event WrittenInTheStars Cards.writtenInTheStars

instance RunMessage WrittenInTheStars where
  runMessage msg e@(WrittenInTheStars attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ DiscardTopOfDeck iid 1 (attrs.ability 1) (Just $ toTarget attrs)
      pure e
    DiscardedTopOfDeck iid cards _ (isTarget attrs -> True) -> do
      let (weaknesses, otherCards) = partition (`cardMatch` WeaknessCard) cards
      unless (null weaknesses)
        $ push
        $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) (map toCard weaknesses)
      for_ otherCards \card -> do
        createCardEffect
          Cards.writtenInTheStars
          (Just $ EffectMetaTarget (toTarget iid))
          attrs
          (CardIdTarget $ toCardId card)
      pure e
    _ -> WrittenInTheStars <$> liftRunMessage msg attrs

newtype WrittenInTheStarsEffect = WrittenInTheStarsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writtenInTheStarsEffect :: EffectArgs -> WrittenInTheStarsEffect
writtenInTheStarsEffect = cardEffect WrittenInTheStarsEffect Cards.writtenInTheStars

instance HasModifiersFor WrittenInTheStarsEffect where
  getModifiersFor (WrittenInTheStarsEffect attrs) = case attrs.target of
    CardIdTarget cid -> case attrs.meta of
      Just (EffectMetaTarget (InvestigatorTarget iid)) -> do
        card <- getCard cid
        committable <- getIsCommittable iid card
        committedCards <- field InvestigatorCommittedCards iid
        (<>)
          <$> modifiedWhen_ attrs (committable || card `elem` committedCards) attrs.target [MustBeCommitted]
          <*> modifiedWhen_ attrs committable iid [CanCommitToSkillTestsAsIfInHand card]
      _ -> error "Expected EffectMetaTarget"
    _ -> pure mempty

instance RunMessage WrittenInTheStarsEffect where
  runMessage msg e@(WrittenInTheStarsEffect attrs) = runQueueT $ case msg of
    EndTurn {} -> do
      disable attrs
      pure e
    _ -> WrittenInTheStarsEffect <$> liftRunMessage msg attrs
