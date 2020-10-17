{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Attrs where

import Arkham.Import

import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

data Attrs = Attrs
  { agendaDoom          :: Int
  , agendaDoomThreshold :: GameValue Int
  , agendaId            :: AgendaId
  , agendaName          :: Text
  , agendaSequence      :: Text
  , agendaAbilities :: [Ability]
  , agendaFlipped :: Bool
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "agenda"
  toEncoding = genericToEncoding $ aesonOptions $ Just "agenda"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "agenda"

doom :: Lens' Attrs Int
doom = lens agendaDoom $ \m x -> m { agendaDoom = x }

sequence :: Lens' Attrs Text
sequence = lens agendaSequence $ \m x -> m { agendaSequence = x }

flipped :: Lens' Attrs Bool
flipped = lens agendaFlipped $ \m x -> m { agendaFlipped = x }

doomThreshold :: Lens' Attrs (GameValue Int)
doomThreshold =
  lens agendaDoomThreshold $ \m x -> m { agendaDoomThreshold = x }

baseAttrs :: AgendaId -> Text -> Text -> GameValue Int -> Attrs
baseAttrs aid name seq' threshold = Attrs
  { agendaDoom = 0
  , agendaDoomThreshold = threshold
  , agendaId = aid
  , agendaName = name
  , agendaSequence = seq'
  , agendaAbilities = mempty
  , agendaFlipped = False
  }

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance AgendaRunner env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doom +~ n
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThreshold)
      totalDoom <- asks $ unDoomCount . getCount ()
      a <$ when
        (totalDoom >= perPlayerDoomThreshold)
        (unshiftMessage (AdvanceAgenda agendaId))
    _ -> pure a
