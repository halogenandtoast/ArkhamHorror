module Arkham.Act.Cards.BeyondTheWitchHouse (BeyondTheWitchHouse (..), beyondTheWitchHouse) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype BeyondTheWitchHouse = BeyondTheWitchHouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondTheWitchHouse :: ActCard BeyondTheWitchHouse
beyondTheWitchHouse =
  act
    (2, A)
    BeyondTheWitchHouse
    Cards.beyondTheWitchHouse
    (Just $ GroupClueCost (PerPlayer 5) (locationIs Locations.witchHouseRuins))

instance RunMessage BeyondTheWitchHouse where
  runMessage msg a@(BeyondTheWitchHouse attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      site <- placeSetAsideLocation Locations.siteOfTheSacrifice

      nahab <-
        selectOne (enemyIs Enemies.nahab) >>= \case
          Nothing -> findUniqueCard Enemies.nahab >>= (`createEnemyAt` site)
          Just x -> x <$ enemyMoveTo x site

      step <- getCurrentAgendaStep
      if step == 4
        then do
          agendaId <- selectJust AnyAgenda
          agendaDoom <- field AgendaDoom agendaId
          removeAllDoom attrs agendaId
          placeDoom attrs nahab agendaDoom
        else placeDoom attrs nahab 2

      selectOne (enemyIs Enemies.brownJenkin) >>= \case
        Nothing -> findUniqueCard Enemies.brownJenkin >>= (`createEnemyAt_` site)
        Just brownJenkin -> enemyMoveTo brownJenkin site

      advanceActDeck attrs
      pure a
    _ -> BeyondTheWitchHouse <$> liftRunMessage msg attrs
