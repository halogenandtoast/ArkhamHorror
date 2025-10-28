module Arkham.Agenda.Cards.LostMemories (lostMemories) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype LostMemories = LostMemories AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: AgendaCard LostMemories
lostMemories = agenda (2, A) LostMemories Cards.lostMemories (Static 7)

instance HasModifiersFor LostMemories where
  getModifiersFor (LostMemories attrs) = when (onSide A attrs) do
    modifySelect attrs Anyone [HandSize (-2)]

instance RunMessage LostMemories where
  runMessage msg a@(LostMemories attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn

      whenAny (assetIs Assets.theCustodian <> UncontrolledAsset) do
        custodian <- selectJust $ assetIs Assets.theCustodian
        locationWithMostClues <- select $ LocationWithMostClues Anywhere
        leadChooseOrRunOneM $ targets locationWithMostClues $ place custodian . AtLocation

      getInvestigatorsWithSupply Pendant >>= traverse_ \iid -> do
        gameModifier attrs iid IgnoreHandSizeReduction
        drawCards iid attrs 2
      advanceAgendaDeck attrs
      pure a
    _ -> LostMemories <$> liftRunMessage msg attrs
