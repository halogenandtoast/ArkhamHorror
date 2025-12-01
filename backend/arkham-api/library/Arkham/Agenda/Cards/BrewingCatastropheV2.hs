module Arkham.Agenda.Cards.BrewingCatastropheV2 (brewingCatastropheV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DogsOfWar.Helpers

newtype BrewingCatastropheV2 = BrewingCatastropheV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brewingCatastropheV2 :: AgendaCard BrewingCatastropheV2
brewingCatastropheV2 = agenda (1, A) BrewingCatastropheV2 Cards.brewingCatastropheV2 (Static 14)

instance HasModifiersFor BrewingCatastropheV2 where
  getModifiersFor (BrewingCatastropheV2 a) = do
    locationWithKeyLocus <-
      select
        $ LocationWithAsset
        $ mapOneOf assetIs [Assets.keyLocusLastBastion, Assets.keyLocusDefensiveBarrier]
    if null locationWithKeyLocus
      then modifySelect a (location_ "The Bourse") [ScenarioModifier "keyLocus"]
      else modifyEach a locationWithKeyLocus [ScenarioModifier "keyLocus"]

instance RunMessage BrewingCatastropheV2 where
  runMessage msg a@(BrewingCatastropheV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      leadChooseOneM $ scenarioI18n do
        labeled' "brewingCatastrophe.trauma" do
          eachInvestigator \iid -> do
            directDamage iid attrs 1
            sufferPhysicalTrauma iid 1
        labeled' "brewingCatastrophe.resign" do
          eachInvestigator resign
      revertAgenda attrs
      placeDoomOnAgenda 10
      pure a
    _ -> BrewingCatastropheV2 <$> liftRunMessage msg attrs
