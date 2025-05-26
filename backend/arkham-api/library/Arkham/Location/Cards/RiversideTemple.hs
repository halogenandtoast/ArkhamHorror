module Arkham.Location.Cards.RiversideTemple (riversideTemple) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype RiversideTemple = RiversideTemple LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riversideTemple :: LocationCard RiversideTemple
riversideTemple = symbolLabel $ location RiversideTemple Cards.riversideTemple 1 (PerPlayer 1)

instance HasAbilities RiversideTemple where
  getAbilities (RiversideTemple a) =
    extendRevealed1 a
      $ restricted a 1 (not_ $ HasSupply Chalk)
      $ forced
      $ DiscoverClues #after Anyone (be a) (atLeast 1)

instance RunMessage RiversideTemple where
  runMessage msg l@(RiversideTemple attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      cards <- take n . unDeck <$> getEncounterDeck
      case cards of
        [] -> pure ()
        xs -> shuffleCardsIntoDeck ExplorationDeck xs
      pure l
    _ -> RiversideTemple <$> liftRunMessage msg attrs
