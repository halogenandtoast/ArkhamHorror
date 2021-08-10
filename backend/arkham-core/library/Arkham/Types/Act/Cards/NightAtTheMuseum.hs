module Arkham.Types.Act.Cards.NightAtTheMuseum
  ( NightAtTheMuseum(..)
  , nightAtTheMuseum
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Target

newtype NightAtTheMuseum = NightAtTheMuseum ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

nightAtTheMuseum :: ActCard NightAtTheMuseum
nightAtTheMuseum = act (2, A) NightAtTheMuseum Cards.nightAtTheMuseum Nothing

instance ActRunner env => RunMessage env NightAtTheMuseum where
  runMessage msg a@(NightAtTheMuseum attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push (chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)])
      pure $ NightAtTheMuseum $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorror of
        Just eid -> do
          lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
            <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          a <$ pushAll
            [ EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , NextAct actId "02125"
            ]
        Nothing -> a <$ push
          (FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardWithCardCode "02141")
          )
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- getJustLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ pushAll [EnemySpawnFromVoid Nothing lid eid, NextAct actId "02125"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- getJustLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ pushAll [SpawnEnemyAt (EncounterCard ec) lid, NextAct actId "02125"]
    WhenEnterLocation _ lid -> do
      mRestrictedHallId <- getLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ when
        (Just lid == mRestrictedHallId)
        (push $ AdvanceAct actId (toSource attrs))
    _ -> NightAtTheMuseum <$> runMessage msg attrs
