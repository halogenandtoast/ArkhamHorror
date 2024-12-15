{-# LANGUAGE TemplateHaskell #-}

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
import Data.Data
import GHC.Records

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ AgendaId
  , EntityAttrs a ~ AgendaAttrs
  ) =>
  IsAgenda a

type AgendaCard a = CardBuilder (Int, AgendaId) a

data instance Field Agenda :: Type -> Type where
  AgendaCard :: Field Agenda Card
  AgendaSequence :: Field Agenda AS.AgendaSequence
  AgendaDoom :: Field Agenda Int
  AgendaDeckId :: Field Agenda Int
  AgendaAbilities :: Field Agenda [Ability]
  AgendaUsedWheelOfFortuneX :: Field Agenda Bool

data AgendaAttrs = AgendaAttrs
  { agendaDoom :: Int
  , agendaDoomThreshold :: Maybe GameValue
  , agendaId :: AgendaId
  , agendaCardId :: CardId
  , agendaSequence :: AgendaSequence
  , agendaFlipped :: Bool
  , agendaCardsUnderneath :: [Card]
  , agendaDeckId :: Int
  , agendaRemoveDoomMatchers :: RemoveDoomMatchers
  , agendaUsedWheelOfFortuneX :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance AsId Agenda where
  type IdOf Agenda = AgendaId
  asId = toId

instance AsId AgendaAttrs where
  type IdOf AgendaAttrs = AgendaId
  asId = toId

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
  isTarget AgendaAttrs {agendaId} (AgendaTarget aid) = agendaId == aid
  isTarget _ _ = False

instance Sourceable AgendaAttrs where
  toSource = AgendaSource . toId
  isSource AgendaAttrs {agendaId} (AgendaSource aid) = agendaId == aid
  isSource _ _ = False

onSide :: AgendaSide -> AgendaAttrs -> Bool
onSide side AgendaAttrs {..} = agendaSide agendaSequence == side

isSide :: AgendaSide -> AgendaAttrs -> AgendaId -> Bool
isSide side attrs aid = aid == attrs.id && onSide side attrs

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
agendaWith (n, side) f cardDef threshold g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (deckId, aid) ->
        f
          . g
          $ AgendaAttrs
            { agendaDoom = 0
            , agendaDoomThreshold = Just threshold
            , agendaId = aid
            , agendaCardId = cardId
            , agendaSequence = AS.Sequence n side
            , agendaFlipped = False
            , agendaCardsUnderneath = mempty
            , agendaDeckId = deckId
            , agendaRemoveDoomMatchers = defaultRemoveDoomMatchers
            , agendaUsedWheelOfFortuneX = False
            }
    }

instance HasField "id" AgendaAttrs AgendaId where
  getField = agendaId

instance HasField "doom" AgendaAttrs Int where
  getField = agendaDoom

instance HasField "ability" AgendaAttrs (Int -> Source) where
  getField this = toAbilitySource this

instance HasCardDef AgendaAttrs where
  toCardDef e = case lookup (unAgendaId $ agendaId e) allAgendaCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for agenda " <> show (unAgendaId $ agendaId e)

data Agenda = forall a. IsAgenda a => Agenda a

instance Data Agenda where
  gunfold _ _ _ = error "gunfold(Agenda)"
  toConstr _ = error "toConstr(Agenda)"
  dataTypeOf _ = error "dataTypeOf(Agenda)"

instance HasField "id" Agenda AgendaId where
  getField = toId

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
  getModifiersFor (Agenda a) = getModifiersFor a

instance Be AgendaAttrs AgendaMatcher where
  be = AgendaWithId . agendaId

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

instance HasCardDef Agenda where
  toCardDef a = case lookup (toCardCode a) allAgendaCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for agenda " <> show (toId a)

instance HasCardCode Agenda where
  toCardCode = unAgendaId . toId

instance HasCardCode AgendaAttrs where
  toCardCode = unAgendaId . toId

instance IsCard AgendaAttrs where
  toCard = defaultToCard
  toCardId = agendaCardId
  toCardOwner = const Nothing

instance IsCard Agenda where
  toCard = defaultToCard
  toCardId = agendaCardId . toAttrs
  toCardOwner = const Nothing

data SomeAgendaCard = forall a. IsAgenda a => SomeAgendaCard (AgendaCard a)

liftSomeAgendaCard :: (forall a. AgendaCard a -> b) -> SomeAgendaCard -> b
liftSomeAgendaCard f (SomeAgendaCard a) = f a

someAgendaCardCode :: SomeAgendaCard -> CardCode
someAgendaCardCode = liftSomeAgendaCard cbCardCode

makeLensesWith suffixedFields ''AgendaAttrs
