module Arkham.Homebrew.CircusExMortis.Agendas.ScheduleToKeep (scheduleToKeep) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWith, setActiveDuringSetup)
import Arkham.Helpers.Scenario (getScenarioMetaKeyDefault)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorRemainingHealth, InvestigatorRemainingSanity))
import Arkham.Matcher hiding (InvestigatorEliminated)
import Arkham.Message (pattern InvestigatorNoLongerDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection

newtype ScheduleToKeep = ScheduleToKeep AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scheduleToKeep :: AgendaCard ScheduleToKeep
scheduleToKeep = agenda (1, A) ScheduleToKeep Cards.scheduleToKeep (Static 0)

-- | Investigators frozen beneath Blood on the Line.
frozenKey :: Key
frozenKey = "frozen"

getFrozen :: HasGame m => m [InvestigatorId]
getFrozen = getScenarioMetaKeyDefault frozenKey []

-- | Lethal trauma really removes them, so they neither keep their cards nor come back.
isKilledOrInsane :: HasGame m => InvestigatorId -> m Bool
isKilledOrInsane iid = iid <=~> oneOf [KilledInvestigator, InsaneInvestigator]

instance HasModifiersFor ScheduleToKeep where
  getModifiersFor (ScheduleToKeep a) = modifySelfWith a setActiveDuringSetup [CannotBeAdvancedByDoomThreshold]

instance HasAbilities ScheduleToKeep where
  getAbilities (ScheduleToKeep a) =
    [ restricted a 1 (exists $ UneliminatedInvestigator <> NotInvestigator You)
        $ forced
        $ InvestigatorWouldBeDefeated #when ByAny You
    ]

instance RunMessage ScheduleToKeep where
  runMessage msg a@(ScheduleToKeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      frozen <- getFrozen
      setScenarioMetaKey frozenKey (iid : frozen)
      advanceAgenda attrs
      pure a
    -- "Do not remove cards controlled by that investigator from play." Elimination
    -- is what strips them -- the asset, event and treachery runners all key off
    -- InvestigatorEliminated -- so drop that step for a frozen investigator. The
    -- scenario runs ahead of the agendas, so the message it just queued is still
    -- poppable here. The defeat, its trauma, and the defeated flag (which keeps
    -- them out of every query until the act advances) all still stand.
    InvestigatorWhenEliminated _ iid _ -> do
      frozen <- getFrozen
      when (iid `elem` frozen) $ unlessM (isKilledOrInsane iid) do
        don't $ InvestigatorEliminated iid
        -- "moves their investigator mini-card beneath the agenda" -- they leave the
        -- map, so release engaged enemies at the location first, which is all
        -- elimination would have done for them.
        withLocationOf iid \lid -> selectEach (EnemyWithPlacement $ InThreatArea iid) \eid -> do
          place eid lid
          enemyCheckEngagement eid
        place iid (AttachedToAgenda attrs.id)
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) AgendaAdvancedWithDoom -> do
      eachInvestigator \iid -> do
        chooseOneM iid $ withI18n $ countVar 1 do
          labeled "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
          labeled "sufferMentalTrauma" $ sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      revertAgenda attrs
      pure a
    AdvanceAct {} -> do
      frozen <- filterM (fmap not . isKilledOrInsane) =<< getFrozen
      unless (null frozen) $ priority do
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
          -- "Move their investigator minicards to Caboose" -- a placement, not a
          -- move: they are attached to the agenda, so there is nowhere to move from.
          place iid caboose
        setScenarioMetaKey frozenKey ([] :: [InvestigatorId])
      pure a
    _ -> ScheduleToKeep <$> liftRunMessage msg attrs
