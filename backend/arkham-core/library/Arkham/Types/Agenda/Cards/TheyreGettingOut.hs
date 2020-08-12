{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheyreGettingOut where

import Arkham.Json
import Arkham.Types.ActId
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Trait
import Lens.Micro
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet

newtype TheyreGettingOut = TheyreGettingOut Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theyreGettingOut :: TheyreGettingOut
theyreGettingOut = TheyreGettingOut
  $ baseAttrs "01107" "They're Getting Out!" "Agenda 3a" (Static 10)

instance (AgendaRunner env) => RunMessage env TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 3a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage $ Ask leadInvestigatorId (ChooseOne [AdvanceAgenda aid])
      pure $ TheyreGettingOut $ attrs & sequence .~ "Agenda 3b" & flipped .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 3b" -> do
      actIds <- asks (getSet @ActId ())
      if ActId "01108" `elem` actIds || ActId "01109" `elem` actIds
        then a <$ unshiftMessage (Resolution 3)
        else a <$ unshiftMessage NoResolution -- TODO: defeated and suffer trauma
    EndEnemy -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unengagedEnemyIds <- HashSet.map unUnengagedEnemyId <$> asks (getSet ())
      ghoulEnemyIds <- asks (getSet Ghoul)
      parlorEnemyIds <- asks (getSet (LocationId "01115"))
      let
        enemiesToMove =
          (ghoulEnemyIds `intersection` unengagedEnemyIds)
            `difference` parlorEnemyIds
      messages <- for (HashSet.toList enemiesToMove) $ \eid -> do
        locationId <- asks (getId eid)
        closestLocationIds <-
          HashSet.toList . HashSet.map unClosestLocationId <$> asks
            (getSet (locationId, LocationId "01115"))
        case closestLocationIds of
          [] -> pure Nothing
          [x] -> pure $ Just $ EnemyMove eid locationId x
          xs -> pure $ Just $ Ask leadInvestigatorId $ ChooseOne $ map
            (EnemyMove eid locationId)
            xs
      a <$ unshiftMessage
        (Ask leadInvestigatorId $ ChooseOneAtATime (catMaybes messages))
    EndRoundWindow -> do
      parlorGhoulsCount <- unEnemyCount
        <$> asks (getCount (LocationId "01115", [Ghoul]))
      hallwayGhoulsCount <- unEnemyCount
        <$> asks (getCount (LocationId "01112", [Ghoul]))
      a <$ unshiftMessages
        (replicate (parlorGhoulsCount + hallwayGhoulsCount) PlaceDoomOnAgenda)
    _ -> TheyreGettingOut <$> runMessage msg attrs
