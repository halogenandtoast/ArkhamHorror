module Arkham.Location.Cards.TheMoonRoom (theMoonRoom, TheMoonRoom (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Constants
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheMoonRoom = TheMoonRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMoonRoom :: LocationCard TheMoonRoom
theMoonRoom = location TheMoonRoom Cards.theMoonRoom 0 (Static 0)

instance HasAbilities TheMoonRoom where
  getAbilities (TheMoonRoom a) =
    extendRevealed
      a
      [ withTooltip "You don one of the the empty diving suits and dive into the reflecting pool"
          $ resignAction a
          `withCriteria` (Here <> thisExists a (not_ FloodedLocation))
      , restricted a 2 (thisExists a (not_ FullyFloodedLocation))
          $ forced
          $ RevealLocation #after Anyone (be a)
      ]

instance RunMessage TheMoonRoom where
  runMessage msg l@(TheMoonRoom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      increaseThisFloodLevelTo attrs PartiallyFlooded
      pure l
    UseThisAbility iid (isSource attrs -> True) ResignAbility -> do
      ks <- iid.keys
      act <- selectJust AnyAct
      for_ (filter (`elem` [RedKey, BlackKey]) (toList ks)) (placeKey act)
      TheMoonRoom <$> liftRunMessage msg attrs
    _ -> TheMoonRoom <$> liftRunMessage msg attrs
