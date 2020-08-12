{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Attrs where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Ability
import Arkham.Types.AgendaId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.GameValue
import Arkham.Types.Agenda.Runner
import ClassyPrelude
import Lens.Micro

data Attrs = Attrs
  { agendaDoom          :: Int
  , agendaDoomThreshold :: GameValue
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

doomThreshold :: Lens' Attrs GameValue
doomThreshold =
  lens agendaDoomThreshold $ \m x -> m { agendaDoomThreshold = x }

baseAttrs :: AgendaId -> Text -> Text -> GameValue -> Attrs
baseAttrs aid name seq' threshold = Attrs
  { agendaDoom = 0
  , agendaDoomThreshold = threshold
  , agendaId = aid
  , agendaName = name
  , agendaSequence = seq'
  , agendaAbilities = mempty
  , agendaFlipped = False
  }

instance (AgendaRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    PlaceDoomOnAgenda -> pure $ a & doom +~ 1
    AdvanceAgendaIfThresholdSatisfied -> do
      pc <- unPlayerCount <$> asks (getCount ())
      when
        (a ^. doom + 1 > fromGameValue (a ^. doomThreshold) pc)
        (unshiftMessage (AdvanceAgenda agendaId))
      pure a
    _ -> pure a
