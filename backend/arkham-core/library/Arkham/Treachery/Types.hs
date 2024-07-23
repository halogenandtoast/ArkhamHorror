{-# LANGUAGE TemplateHaskell #-}

module Arkham.Treachery.Types where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken (ChaosToken)
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Deck
import Arkham.Id
import Arkham.Json
import Arkham.Keyword
import Arkham.Name
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait
import Arkham.Treachery.Cards
import Data.Aeson.TH
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
  , EntityId a ~ TreacheryId
  , EntityAttrs a ~ TreacheryAttrs
  ) =>
  IsTreachery a

type TreacheryCard a = CardBuilder (InvestigatorId, TreacheryId) a

data instance Field (DiscardedEntity Treachery) :: Type -> Type where
  DiscardedTreacheryKeywords :: Field (DiscardedEntity Treachery) (Set Keyword)

data instance Field Treachery :: Type -> Type where
  TreacheryTokens :: Field Treachery Tokens
  TreacheryClues :: Field Treachery Int
  TreacheryResources :: Field Treachery Int
  TreacheryDoom :: Field Treachery Int
  TreacheryAttachedTarget :: Field Treachery (Maybe Target)
  TreacheryLocation :: Field Treachery (Maybe LocationId)
  TreacheryTraits :: Field Treachery (Set Trait)
  TreacheryKeywords :: Field Treachery (Set Keyword)
  TreacheryAbilities :: Field Treachery [Ability]
  TreacheryCardDef :: Field Treachery CardDef
  TreacheryCard :: Field Treachery Card
  TreacheryCardId :: Field Treachery CardId
  TreacheryCanBeCommitted :: Field Treachery Bool
  TreacheryPlacement :: Field Treachery Placement
  TreacheryDrawnBy :: Field Treachery InvestigatorId
  TreacheryDrawnFrom :: Field Treachery (Maybe DeckSignifier)
  TreacheryOwner :: Field Treachery (Maybe InvestigatorId)

data TreacheryAttrs = TreacheryAttrs
  { treacheryId :: TreacheryId
  , treacheryCardId :: CardId
  , treacheryCardCode :: CardCode
  , treacheryOwner :: Maybe InvestigatorId
  , treacheryTokens :: Tokens
  , treacheryPlacement :: Placement
  , treacheryCanBeCommitted :: Bool
  , treacheryDrawnBy :: InvestigatorId
  , treacheryDrawnFrom :: Maybe DeckSignifier
  , treacheryResolved :: Set InvestigatorId -- who resolved effects on this
  , treacheryDiscardedBy :: Maybe InvestigatorId
  , treacheryMeta :: Value
  , treacherySealedChaosTokens :: [ChaosToken]
  }
  deriving stock (Show, Eq)

instance AsId Treachery where
  type IdOf Treachery = TreacheryId
  asId = toId

instance AsId TreacheryAttrs where
  type IdOf TreacheryAttrs = TreacheryId
  asId = treacheryId

instance HasField "id" TreacheryAttrs TreacheryId where
  getField = treacheryId

instance HasField "meta" TreacheryAttrs Value where
  getField = treacheryMeta

instance HasField "resources" TreacheryAttrs Int where
  getField = treacheryResources

instance HasField "owner" TreacheryAttrs (Maybe InvestigatorId) where
  getField = treacheryOwner

instance HasField "attached" TreacheryAttrs (Maybe Target) where
  getField = treacheryAttachedTarget

instance HasField "ability" TreacheryAttrs (Int -> Source) where
  getField self = toAbilitySource self

instance HasField "placement" TreacheryAttrs Placement where
  getField = treacheryPlacement

treacheryDoom :: TreacheryAttrs -> Int
treacheryDoom = countTokens Doom . treacheryTokens

treacheryClues :: TreacheryAttrs -> Int
treacheryClues = countTokens Clue . treacheryTokens

treacheryHorror :: TreacheryAttrs -> Int
treacheryHorror = countTokens Horror . treacheryTokens

treacheryResources :: TreacheryAttrs -> Int
treacheryResources = countTokens Resource . treacheryTokens

treacheryAttachedTarget :: TreacheryAttrs -> Maybe Target
treacheryAttachedTarget attrs = placementToAttached attrs.placement

treacheryInHandOf :: TreacheryAttrs -> Maybe InvestigatorId
treacheryInHandOf attrs = case attrs.placement of
  HiddenInHand iid -> Just iid
  _ -> Nothing

treacheryOnTopOfDeck :: TreacheryAttrs -> Maybe InvestigatorId
treacheryOnTopOfDeck attrs = case attrs.placement of
  OnTopOfDeck iid -> Just iid
  _ -> Nothing

treacheryInThreatAreaOf :: TreacheryAttrs -> Maybe InvestigatorId
treacheryInThreatAreaOf attrs = case attrs.placement of
  InThreatArea iid -> Just iid
  AttachedToInvestigator iid -> Just iid
  _ -> Nothing

instance HasField "inThreatAreaOf" TreacheryAttrs (Maybe InvestigatorId) where
  getField = treacheryInThreatAreaOf

instance HasCardCode TreacheryAttrs where
  toCardCode = treacheryCardCode

instance HasCardDef TreacheryAttrs where
  toCardDef a = case lookup (treacheryCardCode a) allTreacheryCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for treachery " <> show (treacheryCardCode a)

instance Entity TreacheryAttrs where
  type EntityId TreacheryAttrs = TreacheryId
  type EntityAttrs TreacheryAttrs = TreacheryAttrs
  toId = treacheryId
  toAttrs = id
  overAttrs f = f

instance Named TreacheryAttrs where
  toName = toName . toCardDef

instance Targetable TreacheryAttrs where
  toTarget = TreacheryTarget . toId
  isTarget TreacheryAttrs {treacheryId} (TreacheryTarget tid) =
    treacheryId == tid
  isTarget _ _ = False

instance Sourceable TreacheryAttrs where
  toSource = TreacherySource . toId
  isSource TreacheryAttrs {treacheryId} (TreacherySource tid) =
    treacheryId == tid
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource _ _ = False

instance IsCard TreacheryAttrs where
  toCard = defaultToCard
  toCardId = treacheryCardId
  toCardOwner = treacheryOwner

treacheryOn :: Targetable target => TreacheryAttrs -> target -> Bool
treacheryOn attrs t = toTarget t `elem` treacheryAttachedTarget attrs

treacheryInThreatArea :: InvestigatorId -> TreacheryAttrs -> Bool
treacheryInThreatArea iid attrs = case attrs.placement of
  InThreatArea iid' -> iid == iid'
  _ -> False

treacheryOnEnemy :: EnemyId -> TreacheryAttrs -> Bool
treacheryOnEnemy = flip treacheryOn

treacheryOnLocation :: LocationId -> TreacheryAttrs -> Bool
treacheryOnLocation = flip treacheryOn

treacheryOnAgenda :: AgendaId -> TreacheryAttrs -> Bool
treacheryOnAgenda = flip treacheryOn

treacheryOnAct :: ActId -> TreacheryAttrs -> Bool
treacheryOnAct = flip treacheryOn

withTreacheryEnemy :: TreacheryAttrs -> (EnemyId -> m a) -> m a
withTreacheryEnemy attrs f = case treacheryAttachedTarget attrs of
  Just (EnemyTarget eid) -> f eid
  _ ->
    error $ show (cdName $ toCardDef attrs) <> " must be attached to an enemy"

withTreacheryLocation :: TreacheryAttrs -> (LocationId -> m a) -> m a
withTreacheryLocation attrs f = case treacheryAttachedTarget attrs of
  Just (LocationTarget lid) -> f lid
  _ ->
    error $ show (cdName $ toCardDef attrs) <> " must be attached to a location"

withTreacheryInvestigator :: TreacheryAttrs -> (InvestigatorId -> m a) -> m a
withTreacheryInvestigator attrs f = case attrs.inThreatAreaOf of
  Just iid -> f iid
  _ ->
    error
      $ show (cdName $ toCardDef attrs)
      <> " must be in the threat area of an investigator"

withTreacheryOwner :: TreacheryAttrs -> (InvestigatorId -> m a) -> m a
withTreacheryOwner attrs f = case treacheryOwner attrs of
  Just iid -> f iid
  _ ->
    error
      $ show (cdName $ toCardDef attrs)
      <> " must be owned by an investigator"

treachery
  :: (TreacheryAttrs -> a)
  -> CardDef
  -> CardBuilder (InvestigatorId, TreacheryId) a
treachery f cardDef = treacheryWith f cardDef id

treacheryWith
  :: (TreacheryAttrs -> a)
  -> CardDef
  -> (TreacheryAttrs -> TreacheryAttrs)
  -> CardBuilder (InvestigatorId, TreacheryId) a
treacheryWith f cardDef g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (iid, tid) ->
        f
          . g
          $ TreacheryAttrs
            { treacheryId = tid
            , treacheryCardId = cardId
            , treacheryCardCode = toCardCode cardDef
            , treacheryPlacement = Limbo
            , treacheryOwner =
                if isJust (cdCardSubType cardDef)
                  then Just iid
                  else Nothing
            , treacheryDrawnBy = iid
            , treacheryTokens = mempty
            , treacheryCanBeCommitted = False
            , treacheryDrawnFrom = Nothing
            , treacheryResolved = mempty
            , treacheryDiscardedBy = Nothing
            , treacheryMeta = Null
            , treacherySealedChaosTokens = []
            }
    }

is :: Target -> TreacheryAttrs -> Bool
is (TreacheryTarget tid) t = tid == treacheryId t
is (CardCodeTarget cardCode) t = cardCode == cdCardCode (toCardDef t)
is (CardIdTarget cardId) t = cardId == toCardId t
is _ _ = False

data Treachery = forall a. IsTreachery a => Treachery a

instance Data Treachery where
  gunfold _ _ _ = error "gunfold(Treachery)"
  toConstr _ = error "toConstr(Treachery)"
  dataTypeOf _ = error "dataTypeOf(Treachery)"

instance HasField "owner" Treachery (Maybe InvestigatorId) where
  getField (Treachery a) = attr treacheryOwner a

instance HasField "placement" Treachery Placement where
  getField (Treachery a) = attr treacheryPlacement a

instance Named Treachery where
  toName (Treachery t) = toName (toAttrs t)

instance Eq Treachery where
  Treachery (a :: a) == Treachery (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Treachery where
  show (Treachery a) = show a

instance ToJSON Treachery where
  toJSON (Treachery a) = toJSON a

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Treachery where
  getAbilities (Treachery a) = getAbilities a

instance HasModifiersFor Treachery where
  getModifiersFor target (Treachery a) = getModifiersFor target a

instance HasCardCode Treachery where
  toCardCode = toCardCode . toAttrs

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs
  toId = toId . toAttrs
  toAttrs (Treachery a) = toAttrs a
  overAttrs f (Treachery a) = Treachery $ overAttrs f a

instance Targetable Treachery where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Treachery where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Treachery where
  toCard = toCard . toAttrs
  toCardId = toCardId . toAttrs
  toCardOwner = toCardOwner . toAttrs

data SomeTreacheryCard
  = forall a.
    IsTreachery a =>
    SomeTreacheryCard
      (TreacheryCard a)

liftSomeTreacheryCard
  :: (forall a. TreacheryCard a -> b) -> SomeTreacheryCard -> b
liftSomeTreacheryCard f (SomeTreacheryCard a) = f a

someTreacheryCardCode :: SomeTreacheryCard -> CardCode
someTreacheryCardCode = liftSomeTreacheryCard cbCardCode

makeLensesWith suffixedFields ''TreacheryAttrs

setMeta :: ToJSON a => a -> TreacheryAttrs -> TreacheryAttrs
setMeta a = metaL .~ toJSON a

$(deriveJSON (aesonOptions $ Just "treachery") ''TreacheryAttrs)
