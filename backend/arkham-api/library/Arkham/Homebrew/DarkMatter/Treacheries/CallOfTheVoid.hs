module Arkham.Homebrew.DarkMatter.Treacheries.CallOfTheVoid (callOfTheVoid) where

import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getImpendingDoom, shuffleIntoScanningDeck)
import Arkham.Treachery.Import.Lifted

newtype CallOfTheVoid = CallOfTheVoid TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfTheVoid :: TreacheryCard CallOfTheVoid
callOfTheVoid = treachery CallOfTheVoid Cards.callOfTheVoid

{- | "Surge. Cannot be cancelled or ignored. / Revelation - If there are 3 or more
tally marks under "Impending Doom" in your Campaign Log, shuffle the set aside
The Feaster from Afar enemy into the scanning deck. Then, remove Call of the Void
from the game."
-}
instance RunMessage CallOfTheVoid where
  runMessage msg t@(CallOfTheVoid attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      doom <- getImpendingDoom
      when (doom >= 3) do
        feaster <- getSetAsideCard Enemies.theFeasterFromAfar
        shuffleIntoScanningDeck [feaster]
      removeFromGame attrs
      pure t
    _ -> CallOfTheVoid <$> liftRunMessage msg attrs
