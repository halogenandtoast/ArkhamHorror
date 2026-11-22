module Arkham.Event.Events.DreamQuest (dreamQuest) where

import Arkham.Card
import Arkham.Cost
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Dreaming.Lifted (flippableAssets)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.I18n
import Arkham.Strategy

newtype DreamQuest = DreamQuest EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamQuest :: EventCard DreamQuest
dreamQuest = event DreamQuest Cards.dreamQuest

{- | "As an additional cost to do so" -- to play it FROM THE DISCARD PILE. The
zone is the entity, not the cost: the def carries
@cdOutOfPlayEffects = [InDiscardEffect]@, so this instance only runs for the copy
that sits in a discard pile, and 'getCostForCard' folds 'AdditionalCost' out of
the card's own modifiers. A play from hand never sees it.
-}
instance HasModifiersFor DreamQuest where
  getModifiersFor (DreamQuest a) = case a.placement of
    StillInDiscard iid ->
      modified_ a a.cardId [AdditionalCost $ FlipAssetFacedownCost (flippableAssets iid) 2]
    _ -> pure ()

instance RunMessage DreamQuest where
  runMessage msg e@(DreamQuest attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid (is attrs -> True) _ _ _ -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #intellect 2)
      investigate sid iid attrs
      shouldMoveWithSkillTest_ $ doStep 1 msg
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ cardI18n $ scope "dreamQuest" do
        labeled "drawCard" $ drawCardsIfCan iid attrs 1
        labeledI "doNotDraw" nothing
      pure e
    {- "If you do" is read off the payment, not off the zone: the flip is the
    additional cost, so nothing was flipped unless it was actually paid.
    Mirroring 'Exile', the card leaves play without being discarded, so it goes
    to the bottom of the deck instead of the discard pile. -}
    DoStep 1 (InvestigatorPlayEvent iid (is attrs -> True) _ _ _)
      | wasAssetFlippedFacedown attrs.payment -> do
          obtainCard (toCard attrs)
          push $ RemoveFromPlay (toSource attrs)
          putCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid) (toCard attrs)
          pure . DreamQuest $ attrs & afterPlayL .~ DeferDiscard
    _ -> DreamQuest <$> liftRunMessage msg attrs
