module Arkham.Location.Cards.SewerCulvert (sewerCulvert) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (sewerCulvert)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Control.Monad.Writer.Class (tell)
import Data.Map.Monoidal.Strict

newtype SewerCulvert = SewerCulvert LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sewerCulvert :: LocationCard SewerCulvert
sewerCulvert =
  locationWith SewerCulvert Cards.sewerCulvert 1 (Static 0)
    $ revealedConnectedMatchersL
    <>~ ["Sewer Tunnels"]

instance HasModifiersFor SewerCulvert where
  getModifiersFor (SewerCulvert attrs) = do
    sewerTunnels <- select $ LocationWithTitle "Sewer Tunnels"
    mods <- toModifiers attrs [ConnectedToWhen (LocationWithTitle "Sewer Tunnels") (be attrs)]
    tell $ MonoidalMap $ mapFromList [(LocationTarget lid, mods) | lid <- sewerTunnels]

instance HasAbilities SewerCulvert where
  getAbilities (SewerCulvert a) =
    extendRevealed1 a $ locationResignAction a

instance RunMessage SewerCulvert where
  runMessage msg (SewerCulvert attrs) = runQueueT $ case msg of
    _ -> SewerCulvert <$> liftRunMessage msg attrs
