module Arkham.Agenda.Cards.CurseOfTheAbyss (curseOfTheAbyss) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype CurseOfTheAbyss = CurseOfTheAbyss AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfTheAbyss :: AgendaCard CurseOfTheAbyss
curseOfTheAbyss = agenda (2, A) CurseOfTheAbyss Cards.curseOfTheAbyss (Static 6)

instance HasAbilities CurseOfTheAbyss where
  getAbilities (CurseOfTheAbyss a) =
    [restricted a 1 (HasScenarioCount StrengthOfTheAbyss $ EqualTo $ Static 0) $ forced AnyWindow]

instance RunMessage CurseOfTheAbyss where
  runMessage msg a@(CurseOfTheAbyss attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addStrengthOfTheAbyss 1
      eachInvestigator \iid -> do
        drawCards iid (attrs.ability 1) 1
        healHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      addStrengthOfTheAbyss 1
      advanceAgendaDeck attrs
      pure a
    _ -> CurseOfTheAbyss <$> liftRunMessage msg attrs
