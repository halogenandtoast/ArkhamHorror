module Arkham.Agenda.Cards.TheCloverClub (theCloverClub) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Campaign
import Arkham.Helpers.Modifiers
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Trait

newtype TheCloverClub = TheCloverClub AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCloverClub :: AgendaCard TheCloverClub
theCloverClub = agenda (1, A) TheCloverClub Cards.theCloverClub (Static 4)

instance HasModifiersFor TheCloverClub where
  getModifiersFor (TheCloverClub attrs) =
    when (onSide A attrs) $ modifySelect attrs (EnemyWithTrait Criminal) [AddKeyword Aloof]

instance HasAbilities TheCloverClub where
  getAbilities (TheCloverClub x) =
    [ mkAbility x 1 $ forced $ EnemyDealtDamage #when AnyDamageEffect (EnemyWithTrait Criminal) AnySource
    | onSide A x
    ]

instance RunMessage TheCloverClub where
  runMessage msg a@(TheCloverClub attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach (EnemyWithTrait Criminal) enemyCheckEngagement
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs

      completedExtracurricularActivity <- any (`elem` ["02041", "51012"]) <$> getCompletedScenarios
      when completedExtracurricularActivity $ push AdvanceCurrentAgenda
      pure a
    _ -> TheCloverClub <$> liftRunMessage msg attrs
