module Arkham.Agenda.Cards.Vengeance (vengeance) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Helpers.Doom
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Data.List (cycle)

newtype Vengeance = Vengeance AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeance :: AgendaCard Vengeance
vengeance = agendaWith (7, A) Vengeance Cards.vengeance (Static 0) (doomThresholdL .~ Nothing)

instance HasAbilities Vengeance where
  getAbilities (Vengeance a) =
    [ mkAbility a 1 $ forced $ MythosStep AfterCheckDoomThreshold
    , restricted a 2 (notExists UneliminatedInvestigator)
        $ forced
        $ InvestigatorDefeated #when ByAny Anyone
    ]

instance RunMessage Vengeance where
  runMessage msg a@(Vengeance attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doom <- getDoomCount
      investigators <- getInvestigators

      for_ (take doom (cycle investigators)) \iid -> do
        chooseOneM iid $ targeting EncounterDeckTarget $ drawEncounterCards iid attrs 1

      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    _ -> Vengeance <$> liftRunMessage msg attrs
