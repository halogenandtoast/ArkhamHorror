module Arkham.Homebrew.AgesUnwound.Agendas.TheRitualBegins (theRitualBegins) where

import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyCardCode))
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (recordSetInsert, remember)
import Arkham.Projection
import Arkham.Trait (Trait (Elite))

newtype TheRitualBegins = TheRitualBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBegins :: AgendaCard TheRitualBegins
theRitualBegins = agenda (1, A) TheRitualBegins Cards.theRitualBegins (Static 6)

{- | "[action]: Call out for assistance, and hope your benefactor is listening.
Draw the set-aside Aid From Afar story card and resolve its text."
-}
instance HasAbilities TheRitualBegins where
  getAbilities (TheRitualBegins a) = [aidFromAfarAbility a 1]

instance RunMessage TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawAidFromAfar iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      {- "Each investigator tests [willpower] (4). Each investigator who succeeds
      gains an action. Each investigator who fails by 3 or more loses an action."

      The mythos phase is after @Do BeginRound@ has already re-derived everyone's
      remaining actions, so this is a plain gain/loss for the round that is
      starting -- no next-turn modifier needed (contrast agendas 2a and 3a, whose
      Forced fires at the end of a turn). -}
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)

      {- "Choose an unengaged, non-weakness, non-[[Elite]] enemy (not a swarm
      card). Remove that enemy from the game. Then, record in your Campaign Log
      that [the chosen enemy] disappeared unexpectedly." -}
      lead <- getLead
      candidates <-
        select
          $ UnengagedEnemy
          <> NonWeaknessEnemy
          <> not_ (EnemyWithTrait Elite)
          <> not_ IsSwarm
      chooseTargetM lead candidates \eid -> do
        cardCode <- field EnemyCardCode eid
        recordSetInsert DisappearedUnexpectedly [cardCode]
        removeFromGame eid

      {- "For the remainder of the game, the current agenda gains: '[action]: ...
      Aid From Afar ...'. Place this card next to the agenda deck as a reminder,
      then advance to agenda 2a."

      The reminder is carried as scenario memory and read by agendas 2a and 3a,
      which is what makes "the /current/ agenda" true after the deck advances; a
      card placed next to the agenda deck is a 'Card', not an entity, so it could
      not offer the ability itself. -}
      remember theCurrentAgendaCallsForAid

      advanceAgendaDeck attrs
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainActions iid (attrs.ability 1) 1
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 3 -> do
      loseStandardActions iid (attrs.ability 1) 1
      pure a
    _ -> TheRitualBegins <$> liftRunMessage msg attrs
