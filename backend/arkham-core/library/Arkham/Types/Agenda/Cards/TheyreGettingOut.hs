module Arkham.Types.Agenda.Cards.TheyreGettingOut where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.ActId
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Trait

newtype TheyreGettingOut = TheyreGettingOut AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreGettingOut :: AgendaCard TheyreGettingOut
theyreGettingOut =
  agenda (3, A) TheyreGettingOut Cards.theyreGettingOut (Static 10)

instance HasActions TheyreGettingOut
instance HasModifiersFor env TheyreGettingOut

instance AgendaRunner env => RunMessage env TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      actIds <- getSet @ActId ()
      let
        resolution = if any ((`elem` actIds) . ActId) ["01108", "01109"]
          then Resolution 3
          else NoResolution
      a <$ push (ScenarioResolution resolution)
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
        (push (chooseOneAtATime leadInvestigatorId $ catMaybes messages))
    EndRoundWindow -> do
      mParlor <- getLocationIdWithTitle "Parlor"
      mHallway <- getLocationIdWithTitle "Hallway"
      parlorGhoulsCount <- case mParlor of
        Just parlor -> unEnemyCount <$> getCount (parlor, [Ghoul])
        Nothing -> pure 0
      hallwayGhoulsCount <- case mHallway of
        Just hallway -> unEnemyCount <$> getCount (hallway, [Ghoul])
        Nothing -> pure 0
      a <$ pushAll
        (replicate (parlorGhoulsCount + hallwayGhoulsCount) PlaceDoomOnAgenda)
    _ -> TheyreGettingOut <$> runMessage msg attrs
