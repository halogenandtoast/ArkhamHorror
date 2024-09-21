module Arkham.Agenda.Cards.TheShadowOfTheEclipse (TheShadowOfTheEclipse (..), theShadowOfTheEclipse) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Matcher hiding (AssetCard, PlaceUnderneath)
import Arkham.Message.Lifted.Choose

newtype TheShadowOfTheEclipse = TheShadowOfTheEclipse AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadowOfTheEclipse :: AgendaCard TheShadowOfTheEclipse
theShadowOfTheEclipse = agenda (2, A) TheShadowOfTheEclipse Cards.theShadowOfTheEclipse (Static 3)

instance RunMessage TheShadowOfTheEclipse where
  runMessage msg a@(TheShadowOfTheEclipse attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      maskedCarnevaleGoers <- selectWithField AssetCard (AssetWithTitle "Masked Carnevale-Goer")
      lead <- getLead
      chooseOneM lead do
        for_ maskedCarnevaleGoers \(x, card) -> do
          let isInnocent = card.cardCode == Assets.maskedCarnevaleGoer_21.cardCode
          targeting x do
            flipOverBy lead lead x
            when isInnocent $ placeUnderneath AgendaDeckTarget [card]
      doStep 1 msg
      pure a
    Do (AdvanceAgenda (isSide B attrs -> True)) -> do
      selectAny (AssetWithTitle "Masked Carnevale-Goer") >>= \case
        True -> revertAgenda attrs
        False -> advanceAgendaDeck attrs
      pure a
    _ -> TheShadowOfTheEclipse <$> liftRunMessage msg attrs
