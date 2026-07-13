module Arkham.Homebrew.CircusExMortis.Locations.RemoteCabin (remoteCabin) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..), countTokens)

newtype RemoteCabin = RemoteCabin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

remoteCabin :: LocationCard RemoteCabin
remoteCabin =
  location RemoteCabin Cards.remoteCabin 4 (Static 1)

instance HasModifiersFor RemoteCabin where
  getModifiersFor (RemoteCabin a) = do
    -- "Investigators cannot enter Remote Cabin while there is fewer than 2 damage on it."
    let damage = countTokens Damage a.tokens
    modifySelect a Anyone [CannotEnter a.id | damage < 2]

-- TODO(homebrew): "As an additional cost to move from Remote Cabin to a non-[[Woods]]
-- location, place 1 doom on a card you control." This cost is dormant/unreachable given
-- the scenario connections (Remote Cabin only connects to the three rightmost copies of
-- Moonlit Forest, which are all [[Woods]]). No destination-filtered leave-cost primitive
-- exists, so it is intentionally not modeled.

instance RunMessage RemoteCabin where
  runMessage msg (RemoteCabin attrs) = runQueueT $ case msg of
    _ -> RemoteCabin <$> liftRunMessage msg attrs
