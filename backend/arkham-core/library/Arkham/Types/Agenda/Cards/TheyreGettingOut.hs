module Arkham.Types.Agenda.Cards.TheyreGettingOut where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait

newtype TheyreGettingOut = TheyreGettingOut AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreGettingOut :: TheyreGettingOut
theyreGettingOut = TheyreGettingOut
  $ baseAttrs "01107" "They're Getting Out!" (Agenda 3 A) (Static 10)

instance HasActions env TheyreGettingOut where
  getActions i window (TheyreGettingOut x) = getActions i window x

instance HasModifiersFor env TheyreGettingOut where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      actIds <- getSet @ActId ()
      let
        resolution = if any ((`elem` actIds) . ActId) ["01108", "01109"]
          then Resolution 3
          else NoResolution
      a <$ unshiftMessage (ScenarioResolution resolution)
    EndEnemy -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      unengagedEnemyIds <- mapSet unUnengagedEnemyId <$> getSet ()
      mParlor <- getLocationIdWithTitle "Parlor"
      ghoulEnemyIds <- getSet Ghoul
      parlorEnemyIds <- maybe (pure mempty) getSet mParlor
      let
        enemiesToMove =
          (ghoulEnemyIds `intersection` unengagedEnemyIds)
            `difference` parlorEnemyIds
      messages <- for (setToList enemiesToMove) $ \eid -> do
        locationId <- getId eid
        closestLocationIds <- case mParlor of
          Just parlor ->
            map unClosestPathLocationId <$> getSetList (locationId, parlor)
          Nothing -> pure mempty
        case closestLocationIds of
          [] -> pure Nothing
          [x] -> pure $ Just $ EnemyMove eid locationId x
          xs -> pure $ Just $ chooseOne leadInvestigatorId $ map
            (EnemyMove eid locationId)
            xs
      a <$ unless
        (null enemiesToMove || null (catMaybes messages))
        (unshiftMessage
          (chooseOneAtATime leadInvestigatorId $ catMaybes messages)
        )
    EndRoundWindow -> do
      mParlor <- getLocationIdWithTitle "Parlor"
      mHallway <- getLocationIdWithTitle "Hallway"
      parlorGhoulsCount <- case mParlor of
        Just parlor -> unEnemyCount <$> getCount (parlor, [Ghoul])
        Nothing -> pure 0
      hallwayGhoulsCount <- case mHallway of
        Just hallway -> unEnemyCount <$> getCount (hallway, [Ghoul])
        Nothing -> pure 0
      a <$ unshiftMessages
        (replicate (parlorGhoulsCount + hallwayGhoulsCount) PlaceDoomOnAgenda)
    _ -> TheyreGettingOut <$> runMessage msg attrs
