module Arkham.Agenda.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards
import Arkham.Agenda.Sequence
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.Matcher
import Arkham.Name
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Data.Typeable

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ AgendaId, EntityAttrs a ~ AgendaAttrs) => IsAgenda a

type AgendaCard a = CardBuilder (Int, AgendaId) a

data instance Field Agenda :: Type -> Type where
  AgendaCard :: Field Agenda Card
  AgendaSequence :: Field Agenda AS.AgendaSequence
  AgendaDoom :: Field Agenda Int
  AgendaDeckId :: Field Agenda Int
  AgendaAbilities :: Field Agenda [Ability]

data AgendaAttrs = AgendaAttrs
  { agendaDoom :: Int
  , agendaDoomThreshold :: Maybe GameValue
  , agendaId :: AgendaId
  , agendaCardId :: CardId
  , agendaSequence :: AgendaSequence
  , agendaFlipped :: Bool
  , agendaTreacheries :: HashSet TreacheryId
  , agendaCardsUnderneath :: [Card]
  , agendaDeckId :: Int
  , agendaRemoveDoomMatchers :: RemoveDoomMatchers
  }
  deriving stock (Show, Eq, Generic)

removeDoomMatchersL :: Lens' AgendaAttrs RemoveDoomMatchers
removeDoomMatchersL =
  lens agendaRemoveDoomMatchers $ \m x -> m { agendaRemoveDoomMatchers = x }

cardsUnderneathL :: Lens' AgendaAttrs [Card]
cardsUnderneathL =
  lens agendaCardsUnderneath $ \m x -> m { agendaCardsUnderneath = x }

treacheriesL :: Lens' AgendaAttrs (HashSet TreacheryId)
treacheriesL = lens agendaTreacheries $ \m x -> m { agendaTreacheries = x }

doomL :: Lens' AgendaAttrs Int
doomL = lens agendaDoom $ \m x -> m { agendaDoom = x }

doomThresholdL :: Lens' AgendaAttrs (Maybe GameValue)
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

instance Targetable AgendaAttrs where
  toTarget = AgendaTarget . toId
  isTarget AgendaAttrs { agendaId } (AgendaTarget aid) = agendaId == aid
  isTarget _ _ = False

instance Sourceable AgendaAttrs where
  toSource = AgendaSource . toId
  isSource AgendaAttrs { agendaId } (AgendaSource aid) = agendaId == aid
  isSource _ _ = False

onSide :: AgendaSide -> AgendaAttrs -> Bool
onSide side AgendaAttrs {..} = agendaSide agendaSequence == side

agenda
  :: (Int, AgendaSide)
  -> (AgendaAttrs -> a)
  -> CardDef
  -> GameValue
  -> CardBuilder (Int, AgendaId) a
agenda agendaSeq f cardDef threshold =
  agendaWith agendaSeq f cardDef threshold id

agendaWith
  :: (Int, AgendaSide)
  -> (AgendaAttrs -> a)
  -> CardDef
  -> GameValue
  -> (AgendaAttrs -> AgendaAttrs)
  -> CardBuilder (Int, AgendaId) a
agendaWith (n, side) f cardDef threshold g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \cardId (deckId, aid) -> f . g $ AgendaAttrs
    { agendaDoom = 0
    , agendaDoomThreshold = Just threshold
    , agendaId = aid
    , agendaCardId = cardId
    , agendaSequence = AS.Sequence n side
    , agendaFlipped = False
    , agendaTreacheries = mempty
    , agendaCardsUnderneath = mempty
    , agendaDeckId = deckId
    , agendaRemoveDoomMatchers = defaultRemoveDoomMatchers
    }
  }

instance HasCardDef AgendaAttrs where
  toCardDef e = case lookup (unAgendaId $ agendaId e) allAgendaCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for agenda " <> show (unAgendaId $ agendaId e)

data Agenda = forall a . IsAgenda a => Agenda a

instance Eq Agenda where
  (Agenda (a :: a)) == (Agenda (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Agenda where
  show (Agenda a) = show a

instance ToJSON Agenda where
  toJSON (Agenda a) = toJSON a

instance HasAbilities Agenda where
  getAbilities (Agenda a) = getAbilities a

instance HasModifiersFor Agenda where
  getModifiersFor target (Agenda a) = getModifiersFor target a

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  type EntityAttrs Agenda = AgendaAttrs
  toId = toId . toAttrs
  toAttrs (Agenda a) = toAttrs a
  overAttrs f (Agenda a) = Agenda $ overAttrs f a

instance Targetable Agenda where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Agenda where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

data SomeAgendaCard = forall a . IsAgenda a => SomeAgendaCard (AgendaCard a)

liftSomeAgendaCard :: (forall a . AgendaCard a -> b) -> SomeAgendaCard -> b
liftSomeAgendaCard f (SomeAgendaCard a) = f a

someAgendaCardCode :: SomeAgendaCard -> CardCode
someAgendaCardCode = liftSomeAgendaCard cbCardCode

