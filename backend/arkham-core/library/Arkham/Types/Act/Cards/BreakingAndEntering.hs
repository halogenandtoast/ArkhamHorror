module Arkham.Types.Act.Cards.BreakingAndEntering
  ( BreakingAndEntering(..)
  , breakingAndEntering
  ) where

import Arkham.Prelude

import Arkham.PlayerCard
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Target

newtype BreakingAndEntering = BreakingAndEntering ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

breakingAndEntering :: BreakingAndEntering
breakingAndEntering = BreakingAndEntering
  $ baseAttrs "02124" "Breaking and Entering" (Act 2 A) Nothing

instance ActionRunner env => HasActions env BreakingAndEntering where
  getActions i window (BreakingAndEntering x) = getActions i window x

instance (HasName env LocationId, ActRunner env) => RunMessage env BreakingAndEntering where
  runMessage msg a@(BreakingAndEntering attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      haroldWalsted <- PlayerCard <$> genPlayerCard "02138"
      case mHuntingHorror of
        Just eid -> do
          lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
            <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          a <$ unshiftMessages
            [ chooseOne
              leadInvestigatorId
              [ TargetLabel
                  (InvestigatorTarget iid)
                  [AddCampaignCardToDeck iid "02138"]
              | iid <- investigatorIds
              ]
            , EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , NextAct actId "02125"
            ]
        Nothing -> a <$ unshiftMessages
          [ chooseOne
            leadInvestigatorId
            [ TargetLabel
                (InvestigatorTarget iid)
                [TakeControlOfSetAsideAsset iid haroldWalsted]
            | iid <- investigatorIds
            ]
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardMatchByCardCode "02141")
          ]
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ unshiftMessages
        [EnemySpawnFromVoid Nothing lid eid, NextAct actId "02125"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ unshiftMessages
        [SpawnEnemyAt (EncounterCard ec) lid, NextAct actId "02125"]
    WhenEnterLocation _ lid -> do
      name <- getName lid
      a <$ when
        (name == mkFullName "Exhibit Hall" "Restricted Hall")
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    _ -> BreakingAndEntering <$> runMessage msg attrs
