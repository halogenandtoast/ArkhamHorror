module Arkham.Event.Events.CosmicRevelation1 (cosmicRevelation1) where

import {-# SOURCE #-} Arkham.Game (asActive)
import Arkham.Capability
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query
import Arkham.Investigator.Types
import Arkham.Matcher
import Arkham.Projection

newtype CosmicRevelation1 = CosmicRevelation1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicRevelation1 :: EventCard CosmicRevelation1
cosmicRevelation1 = event CosmicRevelation1 Cards.cosmicRevelation1

instance RunMessage CosmicRevelation1 where
  runMessage msg e@(CosmicRevelation1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseOneM iid do
        labeled "Even" $ push $ ForChoice 2 msg
        labeled "Odd" $ push $ ForChoice 1 msg
      pure e
    ForChoice n (PlayThisEvent _iid (is attrs -> True)) -> do
      let matcher = if even n then CardWithEvenCost else CardWithOddCost
      investigators <- inTurnOrder =<< select (investigator_ can.reveal.cards)
      for_ investigators \iid' -> do
        cards <- select $ inHandOf NotForPlay iid' <> basic (NonWeakness <> matcher)
        iattrs <- getAttrs @Investigator iid'
        send $ format iattrs <> " reveals " <> formatAsSentence cards
      for_ investigators (`forTarget` msg)
      pure e
    ForTarget (InvestigatorTarget iid) (ForChoice n (PlayThisEvent _iid (is attrs -> True))) -> do
      let matcher = if even n then CardWithEvenCost else CardWithOddCost
      cards <- asActive iid do
        select
          $ PlayableCard (UnpaidCost NoAction)
          $ inHandOf NotForPlay iid
          <> basic (NonWeakness <> matcher)
      chooseOneM iid do
        whenM (can.draw.cards iid) do
          labeled "draw 1 card" $ drawCards iid attrs 1
        unless (null cards) do
          labeled "play 1 revealed card" do
            focusCards cards $ chooseTargetM iid cards (playCardPayingCost iid)
      pure e
    _ -> CosmicRevelation1 <$> liftRunMessage msg attrs
