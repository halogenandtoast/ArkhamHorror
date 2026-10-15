module Arkham.Homebrew.ThePhantomOfTheOpera.Agendas.FaceToFace (faceToFace) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Agendas qualified as Cards
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype FaceToFace = FaceToFace AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

faceToFace :: AgendaCard FaceToFace
faceToFace = agenda (2, A) FaceToFace Cards.faceToFace (Static 7)

instance RunMessage FaceToFace where
  runMessage msg a@(FaceToFace attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
          countVar 1 $ labeled "sufferMentalTrauma" $ sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> FaceToFace <$> liftRunMessage msg attrs
