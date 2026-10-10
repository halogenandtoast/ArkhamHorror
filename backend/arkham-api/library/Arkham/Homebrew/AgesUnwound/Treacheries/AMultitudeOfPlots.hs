module Arkham.Homebrew.AgesUnwound.Treacheries.AMultitudeOfPlots (aMultitudeOfPlots) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype AMultitudeOfPlots = AMultitudeOfPlots TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aMultitudeOfPlots :: TreacheryCard AMultitudeOfPlots
aMultitudeOfPlots = treachery AMultitudeOfPlots Cards.aMultitudeOfPlots

{- | "Peril. __Revelation__ - You must decide (choose one): Place 1 doom on the
current agenda. This can cause the current agenda to advance. This card gains
surge. Remove this card from the game. -- Add this card to the victory display.
/(This will advance the plans of the Myriad, making subsequent scenarios
harder.)/"

Each copy left in the victory display buys the Myriad a plan: Resolution 1 has
the players record one of three statements per copy. The doom branch is sourced
from the card ('placeDoomOnAgendaAndCheckAdvanceBy') so "would place doom"
windows see a card effect, and the surge is granted before the card leaves --
'gainSurge' marks the draw, which resolves after this revelation either way.
-}
instance RunMessage AMultitudeOfPlots where
  runMessage msg t@(AMultitudeOfPlots attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ campaignI18n do
        labeled "aMultitudeOfPlots.placeDoom" do
          placeDoomOnAgendaAndCheckAdvanceBy attrs 1
          gainSurge attrs
          removeFromGame attrs
        labeled "aMultitudeOfPlots.addToVictory" $ addToVictory iid attrs
      pure t
    _ -> AMultitudeOfPlots <$> liftRunMessage msg attrs
