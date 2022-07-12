module Arkham.Agenda.Attrs where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards
import Arkham.Agenda.Sequence
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasAbilities
import Arkham.Classes.RunMessage.Internal
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Projection
import Arkham.Source
import Arkham.Target

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ AgendaId, EntityAttrs a ~ AgendaAttrs) => IsAgenda a

type AgendaCard a = CardBuilder (Int, AgendaId) a

data instance Field AgendaAttrs :: Type -> Type where
  AgendaSequence :: Field AgendaAttrs AS.AgendaSequence
  AgendaDoom :: Field AgendaAttrs Int
  AgendaAbilities :: Field AgendaAttrs [Ability]

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
  overAttrs f = f

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
