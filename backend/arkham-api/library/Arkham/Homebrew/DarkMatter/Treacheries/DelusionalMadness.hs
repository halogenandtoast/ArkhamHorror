module Arkham.Homebrew.DarkMatter.Treacheries.DelusionalMadness (delusionalMadness) where

import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (treacheryInHandOf)

newtype DelusionalMadness = DelusionalMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delusionalMadness :: TreacheryCard DelusionalMadness
delusionalMadness = treachery DelusionalMadness Cards.delusionalMadness

{- | "Peril. Surge. / Revelation - Secretly move a hidden card from your hand to
another investigator's hand, if possible."
-}
instance RunMessage DelusionalMadness where
  runMessage msg t@(DelusionalMadness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- every treachery placed HiddenInHand is, by construction, a hidden card
      hidden <- select $ treacheryInHandOf iid
      others <- select $ NotInvestigator (InvestigatorWithId iid) <> UneliminatedInvestigator
      unless (null hidden || null others) do
        chooseTargetM iid hidden \tid ->
          chooseTargetM iid others \other ->
            push $ PlaceTreachery tid (HiddenInHand other)
      pure t
    _ -> DelusionalMadness <$> liftRunMessage msg attrs
