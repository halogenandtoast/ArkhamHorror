module Arkham.Agenda.Cards.TheEndOfAllThings (theEndOfAllThings) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype TheEndOfAllThings = TheEndOfAllThings AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndOfAllThings :: AgendaCard TheEndOfAllThings
theEndOfAllThings = agenda (4, A) TheEndOfAllThings Cards.theEndOfAllThings (Static 2)

instance HasAbilities TheEndOfAllThings where
  getAbilities (TheEndOfAllThings x) =
    [ mkAbility x 1 $ forced $ MovedBy #after You Matcher.EncounterCardSource
    , mkAbility x 2 $ forced $ EnemyDefeated #when Anyone ByAny $ EnemyWithTitle "Yog-Sothoth"
    ]

instance RunMessage TheEndOfAllThings where
  runMessage msg a@(TheEndOfAllThings attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      yogSothoth <- selectJust (EnemyWithTitle "Yog-Sothoth")
      eachInvestigator (initiateEnemyAttack yogSothoth attrs)
      revertAgenda attrs
      pure a
    _ -> TheEndOfAllThings <$> liftRunMessage msg attrs
