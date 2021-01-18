{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Agenda.Attrs
  ( module Arkham.Types.Agenda.Attrs
  , module X
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Sequence as X
import Arkham.Types.Game.Helpers

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

makeLensesWith suffixedFields ''Attrs

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

onSide :: AgendaSide -> Attrs -> Bool
onSide side Attrs {..} = agendaSide agendaSequence == side

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
  , HasId LeadInvestigatorId env ()
  )
  => RunMessage env Attrs
  where
  runMessage msg a@Attrs {..} = case msg of
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doomL +~ n
    AttachTreachery tid (AgendaTarget aid) | aid == agendaId ->
      pure $ a & treacheriesL %~ insertSet tid
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [AdvanceAgenda agendaId]
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) B)
        & (flippedL .~ True)
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- unDoomCount <$> getCount ()
      a <$ when
        (totalDoom >= perPlayerDoomThreshold)
        (unshiftMessages [AdvanceAgenda agendaId, RemoveAllDoom])
    _ -> pure a
