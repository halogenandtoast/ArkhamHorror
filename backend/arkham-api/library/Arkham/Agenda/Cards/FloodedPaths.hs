module Arkham.Agenda.Cards.FloodedPaths (floodedPaths) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Projection

newtype FloodedPaths = FloodedPaths AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedPaths :: AgendaCard FloodedPaths
floodedPaths = agenda (1, A) FloodedPaths Cards.floodedPaths (Static 13)

instance HasModifiersFor FloodedPaths where
  getModifiersFor (FloodedPaths a) = do
    -- Each location is connected to each other location on the rows directly
    -- above and below it (a location's row is its grid Pos row).
    modifySelectMapM a Anywhere \loc -> do
      mpos <- field LocationPosition loc
      pure case mpos of
        Nothing -> []
        Just pos ->
          [ ConnectedToWhen
              (LocationWithId loc)
              (mapOneOf LocationInRow [pos.row - 1, pos.row + 1])
          ]

instance HasAbilities FloodedPaths where
  getAbilities (FloodedPaths a) =
    [ restricted a 1 (youExist $ at_ FullyFloodedLocation)
        $ forced
        $ TurnBegins #when You
    ]

instance RunMessage FloodedPaths where
  runMessage msg a@(FloodedPaths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      noResolution
      pure a
    _ -> FloodedPaths <$> liftRunMessage msg attrs
