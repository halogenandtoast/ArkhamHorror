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
import Arkham.Message
import Arkham.Placement
import Arkham.Trait (Trait (Item))

newtype CityOfTheGreatRace = CityOfTheGreatRace AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheGreatRace :: AgendaCard CityOfTheGreatRace
cityOfTheGreatRace =
  agenda (1, A) CityOfTheGreatRace Cards.cityOfTheGreatRace (Static 5)

instance HasModifiersFor CityOfTheGreatRace where
  getModifiersFor (InvestigatorTarget _) (CityOfTheGreatRace attrs)
    | onSide A attrs =
        pure $ toModifiers attrs [CannotPlay $ CardWithTrait Item]
  getModifiersFor _ _ = pure []

instance RunMessage CityOfTheGreatRace where
  runMessage msg a@(CityOfTheGreatRace attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigatorIds
      shouldMoveCustodian <-
        selectAny $ assetIs Assets.theCustodian <> UncontrolledAsset

      custodianMessages <-
        if shouldMoveCustodian
          then do
            lead <- getLeadInvestigatorId
            custodian <- selectJust $ assetIs Assets.theCustodian
            locationWithMostClues <- selectList $ LocationWithMostClues Anywhere
            pure $
              [ chooseOrRunOne
                  lead
                  [ targetLabel lid [PlaceAsset custodian $ AtLocation lid]
                  | lid <- locationWithMostClues
                  ]
              ]
          else pure []

      pushAll $
        [ Search
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          [fromTopOfDeck 9]
          (CardWithTrait Item)
          (DrawFoundUpTo iid 2)
        | iid <- iids
        ]
          <> [ShuffleEncounterDiscardBackIn]
          <> custodianMessages
          <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    _ -> CityOfTheGreatRace <$> runMessage msg attrs
