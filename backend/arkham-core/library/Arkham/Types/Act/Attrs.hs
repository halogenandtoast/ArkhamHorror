{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Attrs
  ( module Arkham.Types.Act.Attrs
  , module X
  )
where

import Arkham.Import

import Arkham.Types.Act.Sequence as X
import Arkham.Types.Game.Helpers
import Arkham.Types.RequiredClues as X

data Attrs = Attrs
  { actId         :: ActId
  , actName       :: Text
  , actSequence   :: ActSequence
  , actRequiredClues :: Maybe RequiredClues
  , actClues :: Maybe Int
  , actTreacheries :: HashSet TreacheryId
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "act"
  toEncoding = genericToEncoding $ aesonOptions $ Just "act"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "act"

instance HasStep ActStep Attrs where
  getStep = actStep . actSequence

instance Entity Attrs where
  type EntityId Attrs = ActId
  toId = actId
  toSource = ActSource . toId
  toTarget = ActTarget . toId
  isSource Attrs { actId } (ActSource aid) = actId == aid
  isSource _ _ = False
  isTarget Attrs { actId } (ActTarget aid) = actId == aid
  isTarget _ _ = False

onSide :: ActSide -> Attrs -> Bool
onSide side Attrs {..} = actSide actSequence == side

sequenceL :: Lens' Attrs ActSequence
sequenceL = lens actSequence $ \m x -> m { actSequence = x }

treacheriesL :: Lens' Attrs (HashSet TreacheryId)
treacheriesL = lens actTreacheries $ \m x -> m { actTreacheries = x }

baseAttrs :: ActId -> Text -> ActSequence -> Maybe RequiredClues -> Attrs
baseAttrs aid name seq' mRequiredClues = Attrs
  { actId = aid
  , actName = name
  , actSequence = seq'
  , actClues = Nothing
  , actRequiredClues = mRequiredClues
  , actTreacheries = mempty
  }

instance ActionRunner env => HasActions env Attrs where
  getActions _ FastPlayerWindow attrs@Attrs {..} = case actRequiredClues of
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

instance (HasQueue env, HasSet InScenarioInvestigatorId env ()) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when (null investigatorIds) (unshiftMessage AllInvestigatorsResigned)
    _ -> pure a
