module Arkham.Act.Cards.BeyondTheWitchHouse (
  BeyondTheWitchHouse (..),
  beyondTheWitchHouse,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.Card
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
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
  runMessage msg a@(BeyondTheWitchHouse attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      siteOfTheSacrifice <- getSetAsideCard Locations.siteOfTheSacrifice
      (siteId, sitePlacement) <- placeLocation siteOfTheSacrifice

      nahab <- findUniqueCard Enemies.nahab
      (nahabId, createNahab) <- createEnemyAt nahab siteId Nothing

      step <- getCurrentAgendaStep
      doomMessages <-
        if step == 4
          then do
            agendaId <- selectJust AnyAgenda
            agendaDoom <- field AgendaDoom agendaId
            pure
              [ RemoveAllDoom (toTarget agendaId)
              , PlaceDoom (toSource attrs) (toTarget nahabId) agendaDoom
              ]
          else pure [PlaceDoom (toSource attrs) (toTarget nahabId) 2]

      brownJenkin <- findUniqueCard Enemies.brownJenkin
      createBrownJenkin <- createEnemyAt_ brownJenkin siteId Nothing

      pushAll $
        [sitePlacement, createNahab]
          <> doomMessages
          <> [createBrownJenkin, advanceActDeck attrs]
      pure a
    _ -> BeyondTheWitchHouse <$> runMessage msg attrs
