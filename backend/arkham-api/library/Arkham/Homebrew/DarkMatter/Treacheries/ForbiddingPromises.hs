module Arkham.Homebrew.DarkMatter.Treacheries.ForbiddingPromises (forbiddingPromises) where

import Arkham.Card
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype ForbiddingPromises = ForbiddingPromises TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddingPromises :: TreacheryCard ForbiddingPromises
forbiddingPromises = treachery ForbiddingPromises Cards.forbiddingPromises

{- | "Revelation - Discard the top X cards of your deck. X is your 'Memories'.
For each copy of a card in your hand that you discarded, discard that card from
your hand. If no copies were discarded, take 1 horror."
-}
instance RunMessage ForbiddingPromises where
  runMessage msg t@(ForbiddingPromises attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      memories <- getMemories iid
      if memories <= 0
        then assignHorror iid attrs 1
        else discardTopOfDeckAndHandle iid attrs memories attrs
      pure t
    DiscardedTopOfDeck iid discarded (isSource attrs -> True) (isTarget attrs -> True) -> do
      hand <- field InvestigatorHand iid
      let discardedTitles = map (cdName . toCardDef) discarded
          matching = filter ((`elem` discardedTitles) . cdName . toCardDef) hand
      if null matching
        then assignHorror iid attrs 1
        else for_ matching $ discardCard iid attrs
      pure t
    _ -> ForbiddingPromises <$> liftRunMessage msg attrs
