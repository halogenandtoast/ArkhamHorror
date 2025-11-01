module Arkham.Event.Events.LeadingLadyEnchantress (leadingLadyEnchantress) where

import Arkham.ClassSymbol
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Playable
import Arkham.Matcher
import Arkham.Modifier

newtype LeadingLadyEnchantress = LeadingLadyEnchantress EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingLadyEnchantress :: EventCard LeadingLadyEnchantress
leadingLadyEnchantress = event LeadingLadyEnchantress Cards.leadingLadyEnchantress

instance RunMessage LeadingLadyEnchantress where
  runMessage msg e@(LeadingLadyEnchantress attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- withoutModifiersFrom iid $ getPlayableCards attrs iid (UnpaidCost NoAction) attrs.windows
      chooseTargetM iid cards \card -> do
        temporaryModifier iid attrs (MetaModifier $ object ["leadingLady" .= card.id]) do
          playCardPayingCost iid card
        when (Mystic `elem` card.classes) $ do_ msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      anyDoom <- select $ TargetControlledBy (affectsColocated iid) <> TargetWithDoom
      chooseOneM iid do
        labeled "Draw 1 card" $ drawCards iid attrs 1
        unless (null anyDoom) $ labeled "Remove 1 doom from a player card at your location" do
          chooseTargetM iid anyDoom $ removeDoomFrom attrs 1
        labeled "Do nothing" nothing
      pure e
    _ -> LeadingLadyEnchantress <$> liftRunMessage msg attrs
