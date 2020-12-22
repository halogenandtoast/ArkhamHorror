{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Attrs
  ( module Arkham.Types.Agenda.Attrs
  , module X
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Sequence as X

data Attrs = Attrs
  { agendaDoom          :: Int
  , agendaDoomThreshold :: GameValue Int
  , agendaId            :: AgendaId
  , agendaName          :: Text
  , agendaSequence      :: AgendaSequence
  , agendaFlipped :: Bool
  , agendaTreacheries :: HashSet TreacheryId
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "agenda"
  toEncoding = genericToEncoding $ aesonOptions $ Just "agenda"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "agenda"

instance Entity Attrs where
  type EntityId Attrs = AgendaId
  toId = agendaId
  toSource = AgendaSource . toId
  toTarget = AgendaTarget . toId
  isSource Attrs { agendaId } (AgendaSource aid) = agendaId == aid
  isSource _ _ = False
  isTarget Attrs { agendaId } (AgendaTarget aid) = agendaId == aid
  isTarget _ _ = False

doomL :: Lens' Attrs Int
doomL = lens agendaDoom $ \m x -> m { agendaDoom = x }

sequenceL :: Lens' Attrs AgendaSequence
sequenceL = lens agendaSequence $ \m x -> m { agendaSequence = x }

flippedL :: Lens' Attrs Bool
flippedL = lens agendaFlipped $ \m x -> m { agendaFlipped = x }

doomThresholdL :: Lens' Attrs (GameValue Int)
doomThresholdL =
  lens agendaDoomThreshold $ \m x -> m { agendaDoomThreshold = x }

treacheriesL :: Lens' Attrs (HashSet TreacheryId)
treacheriesL = lens agendaTreacheries $ \m x -> m { agendaTreacheries = x }

baseAttrs :: AgendaId -> Text -> AgendaSequence -> GameValue Int -> Attrs
baseAttrs aid name seq' threshold = Attrs
  { agendaDoom = 0
  , agendaDoomThreshold = threshold
  , agendaId = aid
  , agendaName = name
  , agendaSequence = seq'
  , agendaFlipped = False
  , agendaTreacheries = mempty
  }

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance HasStep AgendaStep Attrs where
  getStep = agendaStep . agendaSequence

instance HasCount DoomCount env Attrs where
  getCount = pure . DoomCount . agendaDoom

instance
  ( HasQueue env
  , HasCount DoomCount env ()
  , HasCount PlayerCount env ()
  )
  => RunMessage env Attrs
  where
  runMessage msg a@Attrs {..} = case msg of
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doomL +~ n
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- unDoomCount <$> getCount ()
      a <$ when
        (totalDoom >= perPlayerDoomThreshold)
        (unshiftMessages [AdvanceAgenda agendaId, RemoveAllDoom])
    _ -> pure a
