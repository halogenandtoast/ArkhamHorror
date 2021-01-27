module Arkham.Types.Agenda.Cards.TheyreGettingOut where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait

newtype TheyreGettingOut = TheyreGettingOut Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theyreGettingOut :: TheyreGettingOut
theyreGettingOut = TheyreGettingOut
  $ baseAttrs "01107" "They're Getting Out!" (Agenda 3 A) (Static 10)

instance HasActions env TheyreGettingOut where
  getActions i window (TheyreGettingOut x) = getActions i window x

instance HasModifiersFor env TheyreGettingOut where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      actIds <- getSet @ActId ()
      if ActId "01108" `elem` actIds || ActId "01109" `elem` actIds
        then a <$ unshiftMessage (ScenarioResolution $ Resolution 3)
        else a <$ unshiftMessage (ScenarioResolution NoResolution) -- TODO: defeated and suffer trauma
    EndEnemy -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      unengagedEnemyIds <- mapSet unUnengagedEnemyId <$> getSet ()
      mParlor <- getId @(Maybe LocationId) (LocationWithTitle "Parlor")
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
          xs -> pure $ Just $ Ask leadInvestigatorId $ ChooseOne $ map
            (EnemyMove eid locationId)
            xs
      a <$ unless
        (null enemiesToMove || null (catMaybes messages))
        (unshiftMessage
          (Ask leadInvestigatorId $ ChooseOneAtATime (catMaybes messages))
        )
    EndRoundWindow -> do

      mParlor <- getId @(Maybe LocationId) (LocationWithTitle "Parlor")
      mHallway <- getId @(Maybe LocationId) (LocationWithTitle "Hallway")
      parlorGhoulsCount <- case mParlor of
        Just parlor -> unEnemyCount <$> getCount (parlor, [Ghoul])
        Nothing -> pure 0
      hallwayGhoulsCount <- case mHallway of
        Just hallway -> unEnemyCount <$> getCount (hallway, [Ghoul])
        Nothing -> pure 0
      a <$ unshiftMessages
        (replicate (parlorGhoulsCount + hallwayGhoulsCount) PlaceDoomOnAgenda)
    _ -> TheyreGettingOut <$> runMessage msg attrs
