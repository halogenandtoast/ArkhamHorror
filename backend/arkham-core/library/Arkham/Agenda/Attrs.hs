module Arkham.Agenda.Attrs
  ( module Arkham.Agenda.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards
import Arkham.Json
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Sequence as X
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Id
import Arkham.Message
import Arkham.Name
import Arkham.Query
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

class IsAgenda a

type AgendaCard a = CardBuilder (Int, AgendaId) a

data AgendaAttrs = AgendaAttrs
  { agendaDoom :: Int
  , agendaDoomThreshold :: GameValue Int
  , agendaId :: AgendaId
  , agendaSequence :: AgendaSequence
  , agendaFlipped :: Bool
  , agendaTreacheries :: HashSet TreacheryId
  , agendaCardsUnderneath :: [Card]
  , agendaDeckId :: Int
  }
  deriving stock (Show, Eq, Generic)

cardsUnderneathL :: Lens' AgendaAttrs [Card]
cardsUnderneathL =
  lens agendaCardsUnderneath $ \m x -> m { agendaCardsUnderneath = x }

treacheriesL :: Lens' AgendaAttrs (HashSet TreacheryId)
treacheriesL = lens agendaTreacheries $ \m x -> m { agendaTreacheries = x }

doomL :: Lens' AgendaAttrs Int
doomL = lens agendaDoom $ \m x -> m { agendaDoom = x }

doomThresholdL :: Lens' AgendaAttrs (GameValue Int)
doomThresholdL =
  lens agendaDoomThreshold $ \m x -> m { agendaDoomThreshold = x }

sequenceL :: Lens' AgendaAttrs AgendaSequence
sequenceL = lens agendaSequence $ \m x -> m { agendaSequence = x }

flippedL :: Lens' AgendaAttrs Bool
flippedL = lens agendaFlipped $ \m x -> m { agendaFlipped = x }

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

instance Named AgendaAttrs where
  toName = toName . toCardDef

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

agenda
  :: (Int, AgendaSide)
  -> (AgendaAttrs -> a)
  -> CardDef
  -> GameValue Int
  -> CardBuilder (Int, AgendaId) a
agenda agendaSeq f cardDef threshold =
  agendaWith agendaSeq f cardDef threshold id

agendaWith
  :: (Int, AgendaSide)
  -> (AgendaAttrs -> a)
  -> CardDef
  -> GameValue Int
  -> (AgendaAttrs -> AgendaAttrs)
  -> CardBuilder (Int, AgendaId) a
agendaWith (n, side) f cardDef threshold g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(deckId, aid) -> f . g $ AgendaAttrs
    { agendaDoom = 0
    , agendaDoomThreshold = threshold
    , agendaId = aid
    , agendaSequence = AS.Agenda n side
    , agendaFlipped = False
    , agendaTreacheries = mempty
    , agendaCardsUnderneath = mempty
    , agendaDeckId = deckId
    }
  }

instance HasCardDef AgendaAttrs where
  toCardDef e = case lookup (unAgendaId $ agendaId e) allAgendaCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for agenda " <> show (unAgendaId $ agendaId e)

instance HasStep AgendaStep env AgendaAttrs where
  getStep = pure . agendaStep . agendaSequence

instance HasList UnderneathCard env AgendaAttrs where
  getList = pure . map UnderneathCard . agendaCardsUnderneath

instance HasCount DoomCount env AgendaAttrs where
  getCount = pure . DoomCount . agendaDoom

instance
  ( HasQueue env
  , HasCount DoomCount env ()
  , HasCount PlayerCount env ()
  , HasId LeadInvestigatorId env ()
  , HasSet InvestigatorId env ()
  )
  => RunMessage env AgendaAttrs
  where
  runMessage msg a@AgendaAttrs {..} = case msg of
    PlaceUnderneath target cards | isTarget a target ->
      pure $ a & cardsUnderneathL %~ (<> cards)
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doomL +~ n
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    AttachTreachery tid (AgendaTarget aid) | aid == agendaId ->
      pure $ a & treacheriesL %~ insertSet tid
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne leadInvestigatorId [AdvanceAgenda agendaId]
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) B)
        & (flippedL .~ True)
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- unDoomCount <$> getCount ()
      when
        (totalDoom >= perPlayerDoomThreshold)
        do
          whenMsg <- checkWindows
            [ Window
                Timing.When
                (Window.AgendaWouldAdvance DoomThreshold $ toId a)
            ]
          afterMsg <- checkWindows
            [ Window
                Timing.After
                (Window.AgendaWouldAdvance DoomThreshold $ toId a)
            ]
          pushAll [whenMsg, afterMsg, Do AdvanceAgendaIfThresholdSatisfied]
      pure a
    Do AdvanceAgendaIfThresholdSatisfied -> do
      -- This status can change due to the above windows so we much check again
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- unDoomCount <$> getCount ()
      when (totalDoom >= perPlayerDoomThreshold) $ do
        leadInvestigatorId <- getLeadInvestigatorId
        pushAll
          [ CheckWindow
            [leadInvestigatorId]
            [Window Timing.When (Window.AgendaAdvance agendaId)]
          , AdvanceAgenda agendaId
          , RemoveAllDoom
          ]
      pure a
    RemoveAllDoom -> do
      pure $ a & doomL .~ 0
    RevertAgenda aid | aid == agendaId && onSide B a ->
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) A)
    _ -> pure a
