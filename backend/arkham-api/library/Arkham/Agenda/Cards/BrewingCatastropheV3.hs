module Arkham.Agenda.Cards.BrewingCatastropheV3 (brewingCatastropheV3) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DogsOfWar.Helpers

newtype BrewingCatastropheV3 = BrewingCatastropheV3 AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brewingCatastropheV3 :: AgendaCard BrewingCatastropheV3
brewingCatastropheV3 = agenda (1, A) BrewingCatastropheV3 Cards.brewingCatastropheV3 (Static 14)

instance HasModifiersFor BrewingCatastropheV3 where
  getModifiersFor (BrewingCatastropheV3 a) = do
    locationWithKeyLocus <-
      select
        $ LocationWithAsset
        $ mapOneOf assetIs [Assets.keyLocusLastBastion, Assets.keyLocusDefensiveBarrier]
    if null locationWithKeyLocus
      then modifySelect a (location_ "Catacombs of Kom el Shoqafa") [ScenarioModifier "keyLocus"]
      else modifyEach a locationWithKeyLocus [ScenarioModifier "keyLocus"]

instance RunMessage BrewingCatastropheV3 where
  runMessage msg a@(BrewingCatastropheV3 attrs) = runQueueT $ case msg of
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
    _ -> BrewingCatastropheV3 <$> liftRunMessage msg attrs
