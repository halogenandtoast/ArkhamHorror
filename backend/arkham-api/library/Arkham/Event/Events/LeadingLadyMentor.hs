module Arkham.Event.Events.LeadingLadyMentor (leadingLadyMentor) where

import Arkham.ClassSymbol
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Action
import Arkham.Helpers.Playable
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype LeadingLadyMentor = LeadingLadyMentor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingLadyMentor :: EventCard LeadingLadyMentor
leadingLadyMentor = event LeadingLadyMentor Cards.leadingLadyMentor

instance RunMessage LeadingLadyMentor where
  runMessage msg e@(LeadingLadyMentor attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- withoutModifiersFrom iid $ getPlayableCards attrs iid (UnpaidCost NoAction) attrs.windows
      chooseTargetM iid cards \card -> do
        temporaryModifier iid attrs (MetaModifier $ object ["leadingLady" .= card.id]) do
          playCardPayingCost iid card
        when (Seeker `elem` card.classes) $ do_ msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      canInvestigate <- hasInvestigateActions iid (attrs.ability 1) (DuringTurn You) (defaultWindows iid)
      chooseOneM iid do
        labeled "Draw 1 card" $ drawCards iid attrs 1
        when canInvestigate $ labeled "Take investigate action" $ performActionAction iid attrs #investigate
        labeled "Do nothing" nothing
      pure e
    _ -> LeadingLadyMentor <$> liftRunMessage msg attrs
