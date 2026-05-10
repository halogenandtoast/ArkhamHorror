module Arkham.Event.Events.Salvage2 (salvage2) where

import Arkham.Card.Cost
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype Salvage2 = Salvage2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

salvage2 :: EventCard Salvage2
salvage2 = event Salvage2 Cards.salvage2

instance RunMessage Salvage2 where
  runMessage msg e@(Salvage2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <-
        select
          $ inDiscardOf iid
          <> basic #item
          <> oneOf [basic CardWithNonZeroCost, PlayableCard (UnpaidCost NoAction) #any]

      focusCards cards do
        chooseOneM iid do
          targets cards \card -> do
            playable <- getIsPlayable iid attrs (UnpaidCost NoAction) (defaultWindows iid) card
            let cost = maybe 0 toPrintedCost card.cost
            chooseOneM iid $ cardI18n $ scope "salvage" do
              when (cost > 0) do
                labeled' "remove" do
                  obtainCard card
                  gainResourcesIfCan iid attrs cost
              when playable do
                labeled' "play" $ playCardPayingCost iid card

      pure e
    _ -> Salvage2 <$> liftRunMessage msg attrs
