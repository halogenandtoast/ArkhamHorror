module Arkham.Homebrew.AgesUnwound.Agendas.OnceMoreUntoTheBreach (onceMoreUntoTheBreach) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Log (getRecordedCardCodes)

newtype OnceMoreUntoTheBreach = OnceMoreUntoTheBreach AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onceMoreUntoTheBreach :: AgendaCard OnceMoreUntoTheBreach
onceMoreUntoTheBreach = agenda (1, A) OnceMoreUntoTheBreach Cards.onceMoreUntoTheBreach (Static 6)

instance RunMessage OnceMoreUntoTheBreach where
  runMessage msg a@(OnceMoreUntoTheBreach attrs) = runQueueT $ scenarioI18n $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)

      codes <- getRecordedCardCodes DisappearedUnexpectedly
      unless (null codes) do
        lead <- getLead
        for_ codes \code -> do
          cards <- getSetAsideCardsMatching (CardWithCardCode code)
          for_ (take 1 cards) \card ->
            selectOne (locationWithInvestigator lead)
              >>= traverse_ (createEnemyAt_ card)

      advanceAgendaDeck attrs
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainActions iid (attrs.ability 1) 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      loseActions iid (attrs.ability 1) 1
      pure a
    _ -> OnceMoreUntoTheBreach <$> liftRunMessage msg attrs
