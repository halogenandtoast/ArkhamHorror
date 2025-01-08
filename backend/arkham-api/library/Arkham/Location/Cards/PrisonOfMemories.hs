module Arkham.Location.Cards.PrisonOfMemories (prisonOfMemories) where

import Arkham.Card
import Arkham.Helpers.Agenda
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype PrisonOfMemories = PrisonOfMemories LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prisonOfMemories :: LocationCard PrisonOfMemories
prisonOfMemories = location PrisonOfMemories Cards.prisonOfMemories 0 (PerPlayer 3)

instance HasModifiersFor PrisonOfMemories where
  getModifiersFor (PrisonOfMemories a) = do
    n <- getCurrentAgendaStep
    modifySelf a [ShroudModifier n]
    clearedOfMirages a mirageCards

mirageCards :: [CardDef]
mirageCards = [Cards.baseCamp, Cards.deckOfTheTheodosia, Cards.universityHalls]

instance HasAbilities PrisonOfMemories where
  getAbilities (PrisonOfMemories a) =
    extendRevealed a [mirage a 1 mirageCards]

instance RunMessage PrisonOfMemories where
  runMessage msg (PrisonOfMemories attrs) = runQueueT $ case msg of
    _ -> PrisonOfMemories <$> mirageRunner Stories.prisonOfMemories mirageCards 1 msg attrs
