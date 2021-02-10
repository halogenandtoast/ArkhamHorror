{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Agenda.Attrs
  ( module Arkham.Types.Agenda.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import Arkham.Types.Agenda.Sequence as X
import Arkham.Types.Game.Helpers

data AgendaAttrs = AgendaAttrs
  { agendaDoom :: Int
  , agendaDoomThreshold :: GameValue Int
  , agendaId :: AgendaId
  , agendaName :: Text
  , agendaSequence :: AgendaSequence
  , agendaFlipped :: Bool
  , agendaTreacheries :: HashSet TreacheryId
  , agendaCardsUnderneath :: [Card]
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''AgendaAttrs

instance ToJSON AgendaAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "agenda"
  toEncoding = genericToEncoding $ aesonOptions $ Just "agenda"

instance FromJSON AgendaAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "agenda"

instance Entity AgendaAttrs where
  type EntityId AgendaAttrs = AgendaId
  type EntityAttrs AgendaAttrs = AgendaAttrs
  toId = agendaId
  toAttrs = id

instance NamedEntity AgendaAttrs where
  toName = mkName . agendaName

instance TargetEntity AgendaAttrs where
  toTarget = AgendaTarget . toId
  isTarget AgendaAttrs { agendaId } (AgendaTarget aid) = agendaId == aid
  isTarget _ _ = False

instance SourceEntity AgendaAttrs where
  toSource = AgendaSource . toId
  isSource AgendaAttrs { agendaId } (AgendaSource aid) = agendaId == aid
  isSource _ _ = False

onSide :: AgendaSide -> AgendaAttrs -> Bool
onSide side AgendaAttrs {..} = agendaSide agendaSequence == side

baseAttrs :: AgendaId -> Text -> AgendaSequence -> GameValue Int -> AgendaAttrs
baseAttrs aid name seq' threshold = AgendaAttrs
  { agendaDoom = 0
  , agendaDoomThreshold = threshold
  , agendaId = aid
  , agendaName = name
  , agendaSequence = seq'
  , agendaFlipped = False
  , agendaTreacheries = mempty
  , agendaCardsUnderneath = mempty
  }

instance HasActions env AgendaAttrs where
  getActions _ _ _ = pure []

instance HasStep AgendaStep AgendaAttrs where
  getStep = agendaStep . agendaSequence

instance HasList UnderneathCard env AgendaAttrs where
  getList = pure . map UnderneathCard . agendaCardsUnderneath

instance HasCount DoomCount env AgendaAttrs where
  getCount = pure . DoomCount . agendaDoom

instance
  ( HasQueue env
  , HasCount DoomCount env ()
  , HasCount PlayerCount env ()
  , HasId LeadInvestigatorId env ()
  )
  => RunMessage env AgendaAttrs
  where
  runMessage msg a@AgendaAttrs {..} = case msg of
    PlaceUnderneath target cards | isTarget a target ->
      pure $ a & cardsUnderneathL %~ (<> cards)
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doomL +~ n
    AttachTreachery tid (AgendaTarget aid) | aid == agendaId ->
      pure $ a & treacheriesL %~ insertSet tid
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessages
        [ CheckWindow leadInvestigatorId [WhenAgendaAdvance agendaId]
        , chooseOne leadInvestigatorId [AdvanceAgenda agendaId]
        ]
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
