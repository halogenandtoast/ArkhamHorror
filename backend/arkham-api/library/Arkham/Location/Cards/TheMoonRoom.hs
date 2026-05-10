module Arkham.Location.Cards.TheMoonRoom (theMoonRoom, TheMoonRoom (..)) where

import Arkham.Ability hiding (resignAction)
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Constants
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ALightInTheFog.Helpers

newtype TheMoonRoom = TheMoonRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMoonRoom :: LocationCard TheMoonRoom
theMoonRoom = location TheMoonRoom Cards.theMoonRoom 3 (Static 0)

instance HasAbilities TheMoonRoom where
  getAbilities (TheMoonRoom a) =
    extendRevealed
      a
      [ scenarioI18n
          ( withI18nTooltip "theMoonRoom.resign"
              $ resignAction a
              `withCriteria` (Here <> thisExists a (not_ FloodedLocation))
          )
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
