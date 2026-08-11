module Arkham.Homebrew.DarkMatter.Treacheries.Scrambled (scrambled) where

import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getScanningDeck, scanIcons)
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype Scrambled = Scrambled TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrambled :: TreacheryCard Scrambled
scrambled = treachery Scrambled Cards.scrambled

{- | "Surge. Revelation - Shuffle the scanning deck. If the top card of the
scanning deck has an icon matching your current location, take 1 damage."
-}
instance RunMessage Scrambled where
  runMessage msg t@(Scrambled attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      shuffled <- shuffle =<< getScanningDeck
      setScenarioDeck ScanningDeck shuffled
      for_ (take 1 shuffled) \card -> do
        lid <- getJustLocation iid
        symbol <- field LocationPrintedSymbol lid
        when (symbol `elem` scanIcons card) $ assignDamage iid attrs 1
      pure t
    _ -> Scrambled <$> liftRunMessage msg attrs
