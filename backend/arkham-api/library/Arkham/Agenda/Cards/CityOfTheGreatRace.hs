module Arkham.Agenda.Cards.CityOfTheGreatRace (cityOfTheGreatRace) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Strategy
import Arkham.Trait (Trait (Item))

newtype CityOfTheGreatRace = CityOfTheGreatRace AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheGreatRace :: AgendaCard CityOfTheGreatRace
cityOfTheGreatRace = agenda (1, A) CityOfTheGreatRace Cards.cityOfTheGreatRace (Static 5)

instance HasModifiersFor CityOfTheGreatRace where
  getModifiersFor (CityOfTheGreatRace attrs) =
    when (onSide A attrs) $ modifySelect attrs Anyone [CannotPlay $ CardWithTrait Item]

instance RunMessage CityOfTheGreatRace where
  runMessage msg a@(CityOfTheGreatRace attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        search iid attrs iid [fromTopOfDeck 9] #item (DrawFoundUpTo iid 2)

      shuffleEncounterDiscardBackIn

      shouldMoveCustodian <- selectAny $ assetIs Assets.theCustodian <> UncontrolledAsset
      when shouldMoveCustodian do
        custodian <- selectJust $ assetIs Assets.theCustodian
        locationWithMostClues <- select $ LocationWithMostClues Anywhere
        leadChooseOrRunOneM do
          targets locationWithMostClues $ place custodian . AtLocation

      advanceAgendaDeck attrs
      pure a
    _ -> CityOfTheGreatRace <$> liftRunMessage msg attrs
