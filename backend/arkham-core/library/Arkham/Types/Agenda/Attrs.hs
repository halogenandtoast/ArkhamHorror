{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Attrs where

import Arkham.Import

import Arkham.Types.Agenda.Helpers

instance HasAttrs Attrs where
  type AttrsT Attrs = Attrs
  toAttrs = id

data Attrs = Attrs
  { agendaDoom          :: Int
  , agendaDoomThreshold :: GameValue Int
  , agendaId            :: AgendaId
  , agendaName          :: Text
  , agendaSequence      :: Text
  , agendaNumber :: Int
  , agendaAbilities :: [Ability]
  , agendaFlipped :: Bool
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

doom :: Lens' Attrs Int
doom = lens agendaDoom $ \m x -> m { agendaDoom = x }

sequence :: Lens' Attrs Text
sequence = lens agendaSequence $ \m x -> m { agendaSequence = x }

flipped :: Lens' Attrs Bool
flipped = lens agendaFlipped $ \m x -> m { agendaFlipped = x }

doomThreshold :: Lens' Attrs (GameValue Int)
doomThreshold =
  lens agendaDoomThreshold $ \m x -> m { agendaDoomThreshold = x }

baseAttrs :: AgendaId -> Int -> Text -> Text -> GameValue Int -> Attrs
baseAttrs aid num name seq' threshold = Attrs
  { agendaDoom = 0
  , agendaDoomThreshold = threshold
  , agendaId = aid
  , agendaName = name
  , agendaSequence = seq'
  , agendaAbilities = mempty
  , agendaFlipped = False
  , agendaNumber = num
  }

instance HasId AgendaId env Attrs where
  getId = pure . agendaId

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance (HasQueue env, HasCount DoomCount env (), HasCount PlayerCount env ()) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doom +~ n
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThreshold)
      totalDoom <- unDoomCount <$> getCount ()
      a <$ when
        (totalDoom >= perPlayerDoomThreshold)
        (unshiftMessages [AdvanceAgenda agendaId, RemoveAllDoom])
    _ -> pure a
