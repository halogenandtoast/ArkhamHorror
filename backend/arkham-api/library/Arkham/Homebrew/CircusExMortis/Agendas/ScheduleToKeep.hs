module Arkham.Homebrew.CircusExMortis.Agendas.ScheduleToKeep (scheduleToKeep) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Scenario (getScenarioMetaKeyDefault)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Investigator.Types (Field (InvestigatorRemainingHealth, InvestigatorRemainingSanity))
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Message (pattern InvestigatorNoLongerDefeated)
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype ScheduleToKeep = ScheduleToKeep AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scheduleToKeep :: AgendaCard ScheduleToKeep
scheduleToKeep = agenda (1, A) ScheduleToKeep Cards.scheduleToKeep (Static 8)

-- | Investigators frozen beneath Blood on the Line.
frozenKey :: Key
frozenKey = "frozen"

getFrozen :: HasGame m => m [InvestigatorId]
getFrozen = getScenarioMetaKeyDefault frozenKey []

instance HasModifiersFor ScheduleToKeep where
  getModifiersFor (ScheduleToKeep a) = modifySelf a [CannotBeAdvancedByDoomThreshold]

instance HasAbilities ScheduleToKeep where
  getAbilities (ScheduleToKeep a) =
    [ restricted a 1 (exists $ UneliminatedInvestigator <> NotInvestigator You)
        $ forced
        $ InvestigatorWouldBeDefeated #when ByAny You
    , mkAbility a 2 $ SilentForcedAbility $ Matcher.InvestigatorDefeated #after ByAny Anyone
    ]

instance RunMessage ScheduleToKeep where
  runMessage msg a@(ScheduleToKeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      frozen <- getFrozen
      setScenarioMetaKey frozenKey (iid : frozen)
      advanceAgendaDeck attrs
      pure a
    -- "Do not remove cards controlled by that investigator from play." Elimination
    -- is what would strip them (and end the scenario), so drop it for the frozen
    -- investigator; the defeat itself, and its trauma, still resolve.
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      frozen <- getFrozen
      when (iid `elem` frozen) $ don'tMatching \case
        InvestigatorWhenEliminated _ iid' _ -> iid == iid'
        _ -> False
      pure a
    AdvanceAct {} | onSide B attrs -> do
      frozen <- getFrozen
      unless (null frozen) do
        caboose <- selectJust $ locationIs Locations.caboose
        for_ frozen \iid -> do
          -- "heals damage and horror until they have at least 3 remaining health and sanity"
          health <- field InvestigatorRemainingHealth iid
          sanity <- field InvestigatorRemainingSanity iid
          healDamage iid attrs (max 0 (3 - health))
          healHorror iid attrs (max 0 (3 - sanity))
          -- "Those investigators are no longer defeated, but still suffer the
          -- trauma from their defeat" -- the trauma was applied when they were
          -- frozen and is not undone here.
          push $ InvestigatorNoLongerDefeated iid
          moveTo attrs iid caboose
        setScenarioMetaKey frozenKey ([] :: [InvestigatorId])
        push $ RevertAgenda (toId attrs)
      pure a
    _ -> ScheduleToKeep <$> liftRunMessage msg attrs
