module Arkham.Types.Act.Cards.NightAtTheMuseum
  ( NightAtTheMuseum(..)
  , nightAtTheMuseum
  ) where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Target

newtype NightAtTheMuseum = NightAtTheMuseum ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

nightAtTheMuseum :: NightAtTheMuseum
nightAtTheMuseum =
  NightAtTheMuseum $ baseAttrs "02123" "Night at the Museum" (Act 2 A) Nothing

instance ActionRunner env => HasActions env NightAtTheMuseum where
  getActions i window (NightAtTheMuseum x) = getActions i window x

instance ActRunner env => RunMessage env NightAtTheMuseum where
  runMessage msg a@(NightAtTheMuseum attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)])
      pure $ NightAtTheMuseum $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorror of
        Just eid -> do
          lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
            <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          a <$ unshiftMessages
            [ EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , NextAct actId "02125"
            ]
        Nothing -> a <$ unshiftMessage
          (FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardMatchByCardCode "02141")
          )
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- getJustLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ unshiftMessages
        [EnemySpawnFromVoid Nothing lid eid, NextAct actId "02125"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- getJustLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ unshiftMessages
        [SpawnEnemyAt (EncounterCard ec) lid, NextAct actId "02125"]
    WhenEnterLocation _ lid -> do
      mRestrictedHallId <- getLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ when
        (Just lid == mRestrictedHallId)
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    _ -> NightAtTheMuseum <$> runMessage msg attrs
