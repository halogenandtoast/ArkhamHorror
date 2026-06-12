module Arkham.Agenda.Cards.GuardianOfTheRelics (guardianOfTheRelics) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Matcher
import Arkham.Scenarios.RelicsOfThePast.Helpers
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype GuardianOfTheRelics = GuardianOfTheRelics AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardianOfTheRelics :: AgendaCard GuardianOfTheRelics
guardianOfTheRelics =
  agenda (2, A) GuardianOfTheRelics Cards.guardianOfTheRelics (StaticWithPerPlayer 12 1)

instance HasAbilities GuardianOfTheRelics where
  getAbilities (GuardianOfTheRelics a) =
    [mkAbility a 1 $ forced $ InvestigatorDefeated #when ByAny Anyone]

instance RunMessage GuardianOfTheRelics where
  runMessage msg a@(GuardianOfTheRelics attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.InvestigatorDefeated _ iid -> shuffleAncientAssetsIntoExplorationDeck iid
        _ -> pure ()
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> GuardianOfTheRelics <$> liftRunMessage msg attrs
