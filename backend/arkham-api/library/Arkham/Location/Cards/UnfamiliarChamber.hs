module Arkham.Location.Cards.UnfamiliarChamber (unfamiliarChamber, UnfamiliarChamber (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype UnfamiliarChamber = UnfamiliarChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfamiliarChamber :: LocationCard UnfamiliarChamber
unfamiliarChamber = locationWith UnfamiliarChamber Cards.unfamiliarChamber 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities UnfamiliarChamber where
  getAbilities (UnfamiliarChamber attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be attrs)
      ]

instance RunMessage UnfamiliarChamber where
  runMessage msg l@(UnfamiliarChamber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    _ -> UnfamiliarChamber <$> liftRunMessage msg attrs
