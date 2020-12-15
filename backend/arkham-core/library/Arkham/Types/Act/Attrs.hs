{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Attrs where

import Arkham.Import

data Attrs = Attrs
  { actCanAdvance :: Bool
  , actId         :: ActId
  , actName       :: Text
  , actSequence   :: Text
  , actFlipped :: Bool
  , actClues :: Maybe Int
  , actTreacheries :: HashSet TreacheryId
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "act"
  toEncoding = genericToEncoding $ aesonOptions $ Just "act"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "act"

instance Entity Attrs where
  type EntityId Attrs = ActId
  toId = actId
  toSource = ActSource . toId
  toTarget = ActTarget . toId
  isSource Attrs { actId } (ActSource aid) = actId == aid
  isSource _ _ = False
  isTarget Attrs { actId } (ActTarget aid) = actId == aid
  isTarget _ _ = False

canAdvanceL :: Lens' Attrs Bool
canAdvanceL = lens actCanAdvance $ \m x -> m { actCanAdvance = x }

sequenceL :: Lens' Attrs Text
sequenceL = lens actSequence $ \m x -> m { actSequence = x }

flippedL :: Lens' Attrs Bool
flippedL = lens actFlipped $ \m x -> m { actFlipped = x }

treacheriesL :: Lens' Attrs (HashSet TreacheryId)
treacheriesL = lens actTreacheries $ \m x -> m { actTreacheries = x }

baseAttrs :: ActId -> Text -> Text -> Attrs
baseAttrs aid name seq' = Attrs
  { actCanAdvance = False
  , actId = aid
  , actName = name
  , actSequence = seq'
  , actFlipped = False
  , actClues = Nothing
  , actTreacheries = mempty
  }

instance HasActions env Attrs where
  getActions _ FastPlayerWindow Attrs {..} =
    pure [ AdvanceAct actId | actCanAdvance ]
  getActions _ _ _ = pure []

instance (HasQueue env, HasSet InScenarioInvestigatorId env ()) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when (null investigatorIds) (unshiftMessage AllInvestigatorsResigned)
    _ -> pure a
