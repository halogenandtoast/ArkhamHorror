module Arkham.Event.Events.LeadingLadyFemmeFatale (leadingLadyFemmeFatale) where

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

newtype LeadingLadyFemmeFatale = LeadingLadyFemmeFatale EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingLadyFemmeFatale :: EventCard LeadingLadyFemmeFatale
leadingLadyFemmeFatale = event LeadingLadyFemmeFatale Cards.leadingLadyFemmeFatale

instance RunMessage LeadingLadyFemmeFatale where
  runMessage msg e@(LeadingLadyFemmeFatale attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- withoutModifiersFrom iid $ getPlayableCards attrs iid (UnpaidCost NoAction) attrs.windows
      chooseTargetM iid cards \card -> do
        temporaryModifier iid attrs (MetaModifier $ object ["leadingLady" .= card.id]) do
          playCardPayingCost iid card
        when (Rogue `elem` card.classes) $ do_ msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      canEvade <- hasEvadeActions iid (DuringTurn You) (defaultWindows iid)
      chooseOneM iid do
        labeled "Draw 1 card" $ drawCards iid attrs 1
        when canEvade $ labeled "Take fight action" $ performActionAction iid attrs #evade
        labeled "Do nothing" nothing
      pure e
    _ -> LeadingLadyFemmeFatale <$> liftRunMessage msg attrs
