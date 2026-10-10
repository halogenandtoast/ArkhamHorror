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

-- | Agenda 1a. The card prints no front text; everything is on /Time Breaking
-- Slowly/.
onceMoreUntoTheBreach :: AgendaCard OnceMoreUntoTheBreach
onceMoreUntoTheBreach = agenda (1, A) OnceMoreUntoTheBreach Cards.onceMoreUntoTheBreach (Static 6)

instance RunMessage OnceMoreUntoTheBreach where
  runMessage msg a@(OnceMoreUntoTheBreach attrs) = runQueueT $ scenarioI18n $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      {- "Each investigator tests [willpower] (4). Each investigator who succeeds
      gains an action. Each investigator who fails loses an action."

      The mythos phase runs after @Do BeginRound@ has already re-derived everyone's
      remaining actions, so these are plain gains and losses for the round that is
      starting --- no next-turn modifier, unlike agendas 2a and 3a whose Forced
      fires at the end of a turn. Scenario III's agenda 1a prints the same test
      with a "fails by 3 or more" threshold; this printing has none. -}
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)

      {- "Check your Campaign Log. If [an enemy] disappeared unexpectedly, spawn
      the set-aside copy of that enemy at the lead investigator's location."

      Scenario III's agenda 1b writes the card code into the
      'DisappearedUnexpectedly' record set; Scenario VI's setup is what sets a
      copy aside. 'selectOne' rather than 'getJustLocation' because a "returned
      to Arkham late" lead is at no location during the first round. -}
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
      loseStandardActions iid (attrs.ability 1) 1
      pure a
    _ -> OnceMoreUntoTheBreach <$> liftRunMessage msg attrs
