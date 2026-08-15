module Arkham.Homebrew.DarkMatter.Treacheries.ParadoxicalThreat (paradoxicalThreat) where

import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  campaignI18n,
  drawFacedownCards,
  placeFacedownInThreatArea,
 )
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype ParadoxicalThreat = ParadoxicalThreat TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradoxicalThreat :: TreacheryCard ParadoxicalThreat
paradoxicalThreat = treachery ParadoxicalThreat Cards.paradoxicalThreat

{- | "Peril. Revelation - You must either (choose one):
- Place the top 3 cards of the encounter deck into your threat area, face-down.
- Add 1 doom to the current agenda and draw 3 face-down cards from your threat area."
-}
instance RunMessage ParadoxicalThreat where
  runMessage msg t@(ParadoxicalThreat attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ campaignI18n do
        labeled' "paradoxicalThreat.placeFacedown" $ placeFacedownInThreatArea iid 3
        labeled' "paradoxicalThreat.doomAndDraw" do
          placeDoomOnAgendaAndCheckAdvanceBy attrs 1
          drawFacedownCards iid 3
      pure t
    _ -> ParadoxicalThreat <$> liftRunMessage msg attrs
