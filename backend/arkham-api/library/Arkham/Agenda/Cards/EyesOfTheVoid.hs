module Arkham.Agenda.Cards.EyesOfTheVoid (eyesOfTheVoid) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose

newtype EyesOfTheVoid = EyesOfTheVoid AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesOfTheVoid :: AgendaCard EyesOfTheVoid
eyesOfTheVoid = agenda (2, A) EyesOfTheVoid Cards.eyesOfTheVoid (Static 6)

instance RunMessage EyesOfTheVoid where
  runMessage msg a@(EyesOfTheVoid attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        assets <-
          selectWithField AssetCard
            $ oneOf [#item, #ally]
            <> assetControlledBy iid
            <> AssetCanLeavePlayByNormalMeans
        chooseOneM iid $ for_ assets \(aid, card) -> targeting aid $ hollow iid card
      advanceAgendaDeck attrs
      pure a
    _ -> EyesOfTheVoid <$> liftRunMessage msg attrs
