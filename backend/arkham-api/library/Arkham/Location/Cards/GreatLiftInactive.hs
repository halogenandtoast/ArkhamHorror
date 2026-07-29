module Arkham.Location.Cards.GreatLiftInactive (greatLiftInactive) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.CourtOfTheAncients.Helpers (greatLiftConnections)

newtype GreatLiftInactive = GreatLiftInactive LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLiftInactive :: LocationCard GreatLiftInactive
greatLiftInactive =
  locationWith GreatLiftInactive Cards.greatLiftInactive 2 (Static 1)
    $ canBeFlippedL
    .~ True

instance HasModifiersFor GreatLiftInactive where
  -- "Great Lift cannot move and is connected to the locations to the right and
  -- left of it, and vice versa." The "cannot move" is the inactive state.
  getModifiersFor (GreatLiftInactive a) = greatLiftConnections a

instance HasAbilities GreatLiftInactive where
  getAbilities (GreatLiftInactive a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage GreatLiftInactive where
  runMessage msg l@(GreatLiftInactive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.greatLiftActive
      pure l
    _ -> GreatLiftInactive <$> liftRunMessage msg attrs
