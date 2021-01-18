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
        then a <$ unshiftMessage (Resolution 3)
        else a <$ unshiftMessage NoResolution -- TODO: defeated and suffer trauma
    EndEnemy -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      unengagedEnemyIds <- mapSet unUnengagedEnemyId <$> getSet ()
      ghoulEnemyIds <- getSet Ghoul
      parlorEnemyIds <- getSet (LocationWithTitle "Parlor")
      let
        enemiesToMove =
          (ghoulEnemyIds `intersection` unengagedEnemyIds)
            `difference` parlorEnemyIds
      messages <- for (setToList enemiesToMove) $ \eid -> do
        locationId <- getId eid
        closestLocationIds <- map unClosestPathLocationId
          <$> getSetList (locationId, LocationWithTitle "Parlor")
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
      parlorGhoulsCount <- unEnemyCount
        <$> getCount (LocationWithTitle "Parlor", [Ghoul])
      hallwayGhoulsCount <- unEnemyCount
        <$> getCount (LocationWithTitle "Hallway", [Ghoul])
      a <$ unshiftMessages
        (replicate (parlorGhoulsCount + hallwayGhoulsCount) PlaceDoomOnAgenda)
    _ -> TheyreGettingOut <$> runMessage msg attrs
