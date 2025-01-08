module Arkham.Location.Cards.DesertedStation (desertedStation) where

import Arkham.Card.CardDef
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype DesertedStation = DesertedStation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desertedStation :: LocationCard DesertedStation
desertedStation = location DesertedStation Cards.desertedStation 3 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.alaskanWilds]

instance HasModifiersFor DesertedStation where
  getModifiersFor (DesertedStation a) = do
    modifySelfWhenM a (selectAny $ treacheryAt a.id) [ShroudModifier 3]
    clearedOfMirages a mirageCards

instance HasAbilities DesertedStation where
  getAbilities (DesertedStation a) =
    extendRevealed a [mirage a 2 mirageCards]

instance RunMessage DesertedStation where
  runMessage msg (DesertedStation attrs) = runQueueT $ case msg of
    _ -> DesertedStation <$> mirageRunner Stories.desertedStation mirageCards 2 msg attrs
