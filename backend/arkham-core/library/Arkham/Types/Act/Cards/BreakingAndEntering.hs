module Arkham.Types.Act.Cards.BreakingAndEntering
  ( BreakingAndEntering(..)
  , breakingAndEntering
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Target

newtype BreakingAndEntering = BreakingAndEntering ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

breakingAndEntering :: ActCard BreakingAndEntering
breakingAndEntering =
  act (2, A) BreakingAndEntering Cards.breakingAndEntering Nothing

instance (HasName env LocationId, ActRunner env) => RunMessage env BreakingAndEntering where
  runMessage msg a@(BreakingAndEntering attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      haroldWalsted <- EncounterCard <$> genEncounterCard Assets.haroldWalsted
      case mHuntingHorror of
        Just eid -> do
          lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
            <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          a <$ pushAll
            [ chooseOne
              leadInvestigatorId
              [ TargetLabel
                  (InvestigatorTarget iid)
                  [AddCampaignCardToDeck iid Assets.haroldWalsted]
              | iid <- investigatorIds
              ]
            , EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , NextAct actId "02125"
            ]
        Nothing -> a <$ pushAll
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
            (CardWithCardCode "02141")
          ]
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ pushAll [EnemySpawnFromVoid Nothing lid eid, NextAct actId "02125"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ pushAll [SpawnEnemyAt (EncounterCard ec) lid, NextAct actId "02125"]
    WhenEnterLocation _ lid -> do
      name <- getName lid
      a <$ when
        (name == mkFullName "Exhibit Hall" "Restricted Hall")
        (push $ AdvanceAct actId (toSource attrs))
    _ -> BreakingAndEntering <$> runMessage msg attrs
