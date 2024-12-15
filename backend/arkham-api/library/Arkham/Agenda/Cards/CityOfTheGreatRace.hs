module Arkham.Agenda.Cards.CityOfTheGreatRace (
  CityOfTheGreatRace (..),
  cityOfTheGreatRace,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Item))

newtype CityOfTheGreatRace = CityOfTheGreatRace AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheGreatRace :: AgendaCard CityOfTheGreatRace
cityOfTheGreatRace =
  agenda (1, A) CityOfTheGreatRace Cards.cityOfTheGreatRace (Static 5)

instance HasModifiersFor CityOfTheGreatRace where
  getModifiersFor (CityOfTheGreatRace attrs) =
    if onSide A attrs
      then modifySelect attrs Anyone [CannotPlay $ CardWithTrait Item]
      else pure mempty

instance RunMessage CityOfTheGreatRace where
  runMessage msg a@(CityOfTheGreatRace attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigators
      shouldMoveCustodian <-
        selectAny $ assetIs Assets.theCustodian <> UncontrolledAsset

      custodianMessages <-
        if shouldMoveCustodian
          then do
            lead <- getLeadPlayer
            custodian <- selectJust $ assetIs Assets.theCustodian
            locationWithMostClues <- select $ LocationWithMostClues Anywhere
            pure
              $ [ chooseOrRunOne
                    lead
                    [ targetLabel lid [PlaceAsset custodian $ AtLocation lid]
                    | lid <- locationWithMostClues
                    ]
                ]
          else pure []

      pushAll
        $ [search iid attrs iid [fromTopOfDeck 9] #item (DrawFoundUpTo iid 2) | iid <- iids]
        <> [ShuffleEncounterDiscardBackIn]
        <> custodianMessages
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    _ -> CityOfTheGreatRace <$> runMessage msg attrs
