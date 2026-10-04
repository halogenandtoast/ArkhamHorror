module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.LostInTheCaves (lostInTheCaves) where

import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Treachery.Import.Lifted

newtype LostInTheCaves = LostInTheCaves TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheCaves :: TreacheryCard LostInTheCaves
lostInTheCaves = treachery LostInTheCaves Cards.lostInTheCaves

instance RunMessage LostInTheCaves where
  runMessage msg t@(LostInTheCaves attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- "(unrevealed if possible)" narrows the choice rather than widening it.
      ls <-
        select
          $ connectedTo (locationWithInvestigator iid)
          <> not_ (LocationWithInvestigator Anyone)
      unrevealed <- filterM (<=~> UnrevealedLocation) ls
      let choices = if null unrevealed then ls else unrevealed
      chooseTargetM iid choices $ moveTo attrs iid
      pure t
    _ -> LostInTheCaves <$> liftRunMessage msg attrs
