module Arkham.Event.Cards.Salvage2 (salvage2, Salvage2 (..)) where

import Arkham.Card.Cost
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayable)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
      choices <- forToSnd cards \card -> evalQueueT $ do
        playable <- getIsPlayable iid attrs (UnpaidCost NoAction) (defaultWindows iid) card
        let cost = maybe 0 toPrintedCost card.cost
        chooseOneM iid do
          when (cost > 0) do
            labeled "Remove that asset from the game and gain resources equal to its cost." do
              obtainCard card
              gainResourcesIfCan iid attrs cost
          when playable do
            labeled "Play that asset (paying its cost)." do
              playCardPayingCost iid card

      focusCards cards \unfocus -> do
        chooseOne iid [targetLabel asset (unfocus : msgs) | (asset, msgs) <- choices]

      pure e
    _ -> Salvage2 <$> liftRunMessage msg attrs
