module Arkham.Types.Act.Attrs where

import Arkham.Import

data Attrs = Attrs
  { actCanAdvance :: Bool
  , actId         :: ActId
  , actName       :: Text
  , actSequence   :: Text
  , actFlipped :: Bool
  , actClues :: Maybe Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "act"
  toEncoding = genericToEncoding $ aesonOptions $ Just "act"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "act"

isSource :: Attrs -> Source -> Bool
isSource Attrs { actId } (ActSource aid) = actId == aid
isSource _ _ = False

toSource :: Attrs -> Source
toSource Attrs { actId } = ActSource actId

canAdvance :: Lens' Attrs Bool
canAdvance = lens actCanAdvance $ \m x -> m { actCanAdvance = x }

sequence :: Lens' Attrs Text
sequence = lens actSequence $ \m x -> m { actSequence = x }

flipped :: Lens' Attrs Bool
flipped = lens actFlipped $ \m x -> m { actFlipped = x }

baseAttrs :: ActId -> Text -> Text -> Attrs
baseAttrs aid name seq' = Attrs
  { actCanAdvance = False
  , actId = aid
  , actName = name
  , actSequence = seq'
  , actFlipped = False
  , actClues = Nothing
  }

instance HasActions env investigator Attrs where
  getActions _ FastPlayerWindow Attrs {..} =
    pure [ AdvanceAct actId | actCanAdvance ]
  getActions _ _ _ = pure []

instance (HasQueue env) => RunMessage env Attrs where
  runMessage _msg a@Attrs {..} = pure a
