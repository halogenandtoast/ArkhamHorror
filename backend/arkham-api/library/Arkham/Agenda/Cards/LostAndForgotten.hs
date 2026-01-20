module Arkham.Agenda.Cards.LostAndForgotten (lostAndForgotten) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose

newtype LostAndForgotten = LostAndForgotten AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostAndForgotten :: AgendaCard LostAndForgotten
lostAndForgotten = agenda (1, A) LostAndForgotten Cards.lostAndForgotten (Static 7)

instance RunMessage LostAndForgotten where
  runMessage msg a@(LostAndForgotten attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        assets <-
          selectWithField AssetCard
            $ oneOf [#item, #ally]
            <> assetControlledBy iid
            <> AssetCanLeavePlayByNormalMeans
        cards <- select $ inHandOf NotForPlay iid <> basic (oneOf [#item, #ally])
        chooseOneM iid do
          for_ assets \(aid, card) -> targeting aid $ hollow iid card
          targets cards (hollow iid)
      advanceAgendaDeck attrs
      pure a
    _ -> LostAndForgotten <$> liftRunMessage msg attrs
