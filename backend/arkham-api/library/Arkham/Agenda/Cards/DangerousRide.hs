module Arkham.Agenda.Cards.DangerousRide (dangerousRide) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectMapM)
import Arkham.I18n
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers

newtype DangerousRide = DangerousRide AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dangerousRide :: AgendaCard DangerousRide
dangerousRide = agenda (2, A) DangerousRide Cards.dangerousRide (Static 14)

instance HasModifiersFor DangerousRide where
  getModifiersFor (DangerousRide a) = do
    modifySelect a (LocationWithAsset StoryAsset) [CannotBeSlidOrSwapped]
    modifySelectMapM a (InPlayEnemy AnyEnemy) \enemy -> do
      connections <- runDefaultMaybeT [] do
        loc <- MaybeT $ getLocationOf enemy
        pos <- MaybeT $ field LocationPosition loc
        locations <- select $ mapOneOf LocationInPosition (adjacentPositions pos)
        pure $ map HunterConnectedTo locations

      pure $ AddKeyword Keyword.Hunter : ResolveHunterTwice : connections

instance RunMessage DangerousRide where
  runMessage msg a@(DangerousRide attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        chooseOneM iid $ withI18n $ countVar 1 do
          labeled' "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
          labeled' "sufferMentalTrauma" $ sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> DangerousRide <$> liftRunMessage msg attrs
