module Arkham.Homebrew.DarkMatter.Treacheries.FathomlessRegrets (fathomlessRegrets) where

import Arkham.Card
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype FathomlessRegrets = FathomlessRegrets TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fathomlessRegrets :: TreacheryCard FathomlessRegrets
fathomlessRegrets = treachery FathomlessRegrets Cards.fathomlessRegrets

{- | "Revelation - Discard the top X cards of your deck. X is your 'Memories'.
Draw each weakness discarded by this effect. If no weaknesses were drawn, take 1
horror."
-}
instance RunMessage FathomlessRegrets where
  runMessage msg t@(FathomlessRegrets attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      memories <- getMemories iid
      if memories <= 0
        then assignHorror iid attrs 1
        else discardTopOfDeckAndHandle iid attrs memories attrs
      pure t
    DiscardedTopOfDeck iid cards (isSource attrs -> True) (isTarget attrs -> True) -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) (map toCard cards)
      if null weaknesses
        then assignHorror iid attrs 1
        else for_ weaknesses $ drawCard iid
      pure t
    _ -> FathomlessRegrets <$> liftRunMessage msg attrs
