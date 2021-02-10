{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Act.Attrs
  ( module Arkham.Types.Act.Attrs
  , module X
  )
where

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


import Arkham.Types.Act.Sequence as X
import Arkham.Types.Game.Helpers
import Arkham.Types.RequiredClues as X

data ActAttrs = ActAttrs
  { actId :: ActId
  , actName :: Text
  , actSequence :: ActSequence
  , actRequiredClues :: Maybe RequiredClues
  , actClues :: Maybe Int
  , actTreacheries :: HashSet TreacheryId
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ActAttrs

instance ToJSON ActAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "act"
  toEncoding = genericToEncoding $ aesonOptions $ Just "act"

instance FromJSON ActAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "act"

instance HasStep ActStep ActAttrs where
  getStep = actStep . actSequence

instance Entity ActAttrs where
  type EntityId ActAttrs = ActId
  type EntityAttrs ActAttrs = ActAttrs
  toId = actId
  toAttrs = id

instance NamedEntity ActAttrs where
  toName = mkName . actName

instance TargetEntity ActAttrs where
  toTarget = ActTarget . toId
  isTarget ActAttrs { actId } (ActTarget aid) = actId == aid
  isTarget _ _ = False

instance SourceEntity ActAttrs where
  toSource = ActSource . toId
  isSource ActAttrs { actId } (ActSource aid) = actId == aid
  isSource _ _ = False

onSide :: ActSide -> ActAttrs -> Bool
onSide side ActAttrs {..} = actSide actSequence == side

baseAttrs :: ActId -> Text -> ActSequence -> Maybe RequiredClues -> ActAttrs
baseAttrs aid name seq' mRequiredClues = ActAttrs
  { actId = aid
  , actName = name
  , actSequence = seq'
  , actClues = Nothing
  , actRequiredClues = mRequiredClues
  , actTreacheries = mempty
  }

instance ActionRunner env => HasActions env ActAttrs where
  getActions _ FastPlayerWindow attrs@ActAttrs {..} = case actRequiredClues of
    Just (RequiredClues requiredClues Nothing) -> do
      totalSpendableClues <- unSpendableClueCount <$> getCount ()
      totalRequiredClues <- getPlayerCountValue requiredClues
      pure
        [ AdvanceAct actId (toSource attrs)
        | totalSpendableClues >= totalRequiredClues
        ]
    Just (RequiredClues requiredClues (Just locationMatcher)) -> do
      mLocationId <- getId @(Maybe LocationId) locationMatcher
      case mLocationId of
        Just lid -> do
          iids <- getSetList @InvestigatorId lid
          totalSpendableClues <- sum
            <$> for iids ((unSpendableClueCount <$>) . getCount)
          totalRequiredClues <- getPlayerCountValue requiredClues
          pure
            [ AdvanceAct actId (toSource attrs)
            | totalSpendableClues >= totalRequiredClues
            ]
        Nothing -> pure []
    Nothing -> pure []
  getActions _ _ _ = pure []

instance (HasQueue env, HasSet InScenarioInvestigatorId env (), HasId LeadInvestigatorId env ()) => RunMessage env ActAttrs where
  runMessage msg a@ActAttrs {..} = case msg of
    AdvanceAct aid source | aid == actId && actSide actSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessages
        [ CheckWindow leadInvestigatorId [WhenActAdvance actId]
        , chooseOne leadInvestigatorId [AdvanceAct actId source]
        ]
      pure $ a & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AttachTreachery tid (ActTarget aid) | aid == actId ->
      pure $ a & treacheriesL %~ insertSet tid
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when (null investigatorIds) (unshiftMessage AllInvestigatorsResigned)
    _ -> pure a
