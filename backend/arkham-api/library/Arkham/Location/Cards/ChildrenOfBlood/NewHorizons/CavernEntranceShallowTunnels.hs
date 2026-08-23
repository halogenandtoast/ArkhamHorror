module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.CavernEntranceShallowTunnels (cavernEntranceShallowTunnels) where

import Arkham.Ability
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CavernEntranceShallowTunnels = CavernEntranceShallowTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernEntranceShallowTunnels :: LocationCard CavernEntranceShallowTunnels
cavernEntranceShallowTunnels =
  symbolLabel
    $ location CavernEntranceShallowTunnels Cards.cavernEntranceShallowTunnels 2 (PerPlayer 1)

instance HasAbilities CavernEntranceShallowTunnels where
  getAbilities (CavernEntranceShallowTunnels a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage CavernEntranceShallowTunnels where
  runMessage msg l@(CavernEntranceShallowTunnels attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      sideChambers <- shuffle =<< getSetAsideCardsMatching (CardWithTitle "Side Chamber")
      placeLabeledLocations_ "sideChamber" sideChambers
      pure l
    _ -> CavernEntranceShallowTunnels <$> liftRunMessage msg attrs
