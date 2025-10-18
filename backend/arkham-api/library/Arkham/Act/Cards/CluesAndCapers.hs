module Arkham.Act.Cards.CluesAndCapers (cluesAndCapers) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations

newtype CluesAndCapers = CluesAndCapers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cluesAndCapers :: ActCard CluesAndCapers
cluesAndCapers = act (1, A) CluesAndCapers Cards.cluesAndCapers (groupClueCost (PerPlayer 2))

instance RunMessage CluesAndCapers where
  runMessage msg a@(CluesAndCapers attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocations_ [Locations.kensingtonGardens, Locations.westminsterAbbey, Locations.bigBen]
      lead <- getLead
      theRedGlovedMan <- fetchCard Enemies.theRedGlovedManShroudedInMystery
      drawCard lead theRedGlovedMan
      advanceToAgendaA attrs Agendas.figuresInTheFog
      advanceActDeck attrs
      pure a
    _ -> CluesAndCapers <$> liftRunMessage msg attrs
