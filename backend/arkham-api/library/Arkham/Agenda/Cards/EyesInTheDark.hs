module Arkham.Agenda.Cards.EyesInTheDark (eyesInTheDark) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers (runExplore, exploreAction_)
import Arkham.Matcher hiding (InvestigatorDefeated)

newtype EyesInTheDark = EyesInTheDark AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesInTheDark :: AgendaCard EyesInTheDark
eyesInTheDark = agenda (2, A) EyesInTheDark Cards.eyesInTheDark (StaticWithPerPlayer 12 1)

instance HasAbilities EyesInTheDark where
  getAbilities (EyesInTheDark a) =
    [restricted a 1 (exists $ YourLocation <> LocationWithoutClues) exploreAction_]

instance RunMessage EyesInTheDark where
  runMessage msg a@(EyesInTheDark attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (toSource attrs)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> EyesInTheDark <$> liftRunMessage msg attrs
