module Arkham.Homebrew.CircusExMortis.Locations.WoodlandOverlook (woodlandOverlook) where

import Arkham.Helpers.Modifiers
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (neighbouringMoonlitForestColumn)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..), countTokens)

newtype WoodlandOverlook = WoodlandOverlook LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

woodlandOverlook :: LocationCard WoodlandOverlook
woodlandOverlook = location WoodlandOverlook Cards.woodlandOverlook 4 (Static 1)

instance HasModifiersFor WoodlandOverlook where
  getModifiersFor (WoodlandOverlook a) = do
    let damage = countTokens Damage a.tokens
    modifySelectWith a Anyone setActiveDuringSetup [CannotEnter a.id | damage < 2]

    let forests = neighbouringMoonlitForestColumn (be a)
    modifySelfWith a setActiveDuringSetup [ConnectedToWhen (be a) forests]
    modifySelectWith a forests setActiveDuringSetup [ConnectedToWhen forests (be a)]

-- TODO(homebrew): "As an additional cost to move from Woodland Overlook to a non-[[Woods]]
-- location, discard a non-weakness asset you control." No destination-filtered leave-cost
-- primitive exists, so it is not modeled; it bites on the move to Circus Encampment
-- ([[Clearing]]), which is reachable via the printed {moon} connection.

instance RunMessage WoodlandOverlook where
  runMessage msg (WoodlandOverlook attrs) = runQueueT $ case msg of
    _ -> WoodlandOverlook <$> liftRunMessage msg attrs
