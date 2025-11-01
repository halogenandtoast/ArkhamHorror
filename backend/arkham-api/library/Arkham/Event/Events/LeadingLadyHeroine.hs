module Arkham.Event.Events.LeadingLadyHeroine (leadingLadyHeroine) where

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

newtype LeadingLadyHeroine = LeadingLadyHeroine EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingLadyHeroine :: EventCard LeadingLadyHeroine
leadingLadyHeroine = event LeadingLadyHeroine Cards.leadingLadyHeroine

instance RunMessage LeadingLadyHeroine where
  runMessage msg e@(LeadingLadyHeroine attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- withoutModifiersFrom iid $ getPlayableCards attrs iid (UnpaidCost NoAction) attrs.windows
      chooseTargetM iid cards \card -> do
        temporaryModifier iid attrs (MetaModifier $ object ["leadingLady" .= card.id]) do
          playCardPayingCost iid card
        when (Guardian `elem` card.classes) $ do_ msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      canFight <- hasFightActions iid (attrs.ability 1) (DuringTurn You) (defaultWindows iid)
      chooseOneM iid do
        labeled "Draw 1 card" $ drawCards iid attrs 1
        when canFight $ labeled "Take fight action" $ performActionAction iid attrs #fight
        labeled "Do nothing" nothing
      pure e
    _ -> LeadingLadyHeroine <$> liftRunMessage msg attrs
