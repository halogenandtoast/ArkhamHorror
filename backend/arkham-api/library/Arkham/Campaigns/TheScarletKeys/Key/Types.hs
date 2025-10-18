{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Key.Types (
  module Arkham.Campaigns.TheScarletKeys.Key.Types,
  module Arkham.Campaigns.TheScarletKeys.Key.Id,
) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards
import Arkham.Campaigns.TheScarletKeys.Key.Id
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Id
import Arkham.Json
import Arkham.Message (Is (..))
import Arkham.Name
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
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
  , EntityId a ~ ScarletKeyId
  , EntityAttrs a ~ ScarletKeyAttrs
  , RunType a ~ a
  ) =>
  IsScarletKey a

type ScarletKeyCard a = CardBuilder (Target, ScarletKeyId) a

data instance Field ScarletKey :: Type -> Type where
  ScarletKeyCard :: Field ScarletKey Card
  ScarletKeyClues :: Field ScarletKey Int
  ScarletKeyPlacement :: Field ScarletKey Placement
  ScarletKeyOtherSide :: Field ScarletKey (Maybe Target)
  ScarletKeyCardsUnderneath :: Field ScarletKey [Card]

data Stability = Stable | Unstable
  deriving stock (Show, Eq, Data)

data ScarletKeyAttrs = ScarletKeyAttrs
  { keyId :: ScarletKeyId
  , keyCardId :: CardId
  , keyPlacement :: Placement
  , keyStability :: Stability
  , keyBearer :: Target
  }
  deriving stock (Show, Eq)

instance Is ScarletKeyAttrs ScarletKeyId where
  is = (==) . toId
  {-# INLINE is #-}

instance AsId ScarletKeyAttrs where
  type IdOf ScarletKeyAttrs = ScarletKeyId
  asId = keyId

instance HasField "cardId" ScarletKeyAttrs CardId where
  getField = keyCardId

instance HasField "id" ScarletKeyAttrs ScarletKeyId where
  getField = keyId

instance HasField "ability" ScarletKeyAttrs (Int -> Source) where
  getField = toAbilitySource

instance HasField "placement" ScarletKeyAttrs Placement where
  getField = keyPlacement

key :: (ScarletKeyAttrs -> a) -> CardDef -> CardBuilder (Target, ScarletKeyId) a
key f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (bearer, kid) ->
        f
          $ ScarletKeyAttrs
            { keyId = kid
            , keyCardId = cardId
            , keyPlacement = Unplaced
            , keyStability = case bearer of
                InvestigatorTarget _ -> Stable
                _ -> Unstable
            , keyBearer = bearer
            }
    }

instance HasCardDef ScarletKeyAttrs where
  toCardDef e = case lookup (unScarletKeyId $ keyId e) allScarletKeyCards of
    Just def -> def
    Nothing -> error $ "missing card def for key " <> show (unScarletKeyId $ keyId e)

instance HasCardCode ScarletKey where
  toCardCode (ScarletKey a) = toCardCode (toAttrs a)
  {-# INLINE toCardCode #-}

instance Entity ScarletKeyAttrs where
  type EntityId ScarletKeyAttrs = ScarletKeyId
  type EntityAttrs ScarletKeyAttrs = ScarletKeyAttrs
  toId = keyId
  toAttrs = id
  overAttrs f = f

instance Named ScarletKey where
  toName = toName . toAttrs

instance Named ScarletKeyAttrs where
  toName = toName . toCardDef

instance Targetable ScarletKeyAttrs where
  toTarget = ScarletKeyTarget . toId
  isTarget ScarletKeyAttrs {keyId} (ScarletKeyTarget sid) = keyId == sid
  isTarget _ _ = False

instance Sourceable ScarletKeyAttrs where
  toSource = ScarletKeySource . toId
  isSource ScarletKeyAttrs {keyId} (ScarletKeySource sid) = keyId == sid
  isSource _ _ = False

data ScarletKey = forall a. IsScarletKey a => ScarletKey a

instance Data ScarletKey where
  gunfold _ _ _ = error "gunfold(ScarletKey)"
  toConstr _ = error "toConstr(ScarletKey)"
  dataTypeOf _ = error "dataTypeOf(ScarletKey)"

instance Eq ScarletKey where
  (ScarletKey (a :: a)) == (ScarletKey (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show ScarletKey where
  show (ScarletKey a) = show a

instance ToJSON ScarletKey where
  toJSON (ScarletKey a) = toJSON a

instance HasAbilities ScarletKey where
  getAbilities (ScarletKey a) = getAbilities a

instance HasModifiersFor ScarletKey where
  getModifiersFor (ScarletKey a) = getModifiersFor a

instance Entity ScarletKey where
  type EntityId ScarletKey = ScarletKeyId
  type EntityAttrs ScarletKey = ScarletKeyAttrs
  toId = toId . toAttrs
  toAttrs (ScarletKey a) = toAttrs a
  overAttrs f (ScarletKey a) = ScarletKey $ overAttrs f a

instance Targetable ScarletKey where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable ScarletKey where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardCode ScarletKeyAttrs where
  toCardCode = unScarletKeyId . keyId

instance IsCard ScarletKeyAttrs where
  toCardId = keyCardId
  toCard a = lookupCard (unScarletKeyId $ keyId a) (toCardId a)
  toCardOwner _ = Nothing

data SomeScarletKeyCard = forall a. IsScarletKey a => SomeScarletKeyCard (ScarletKeyCard a)

liftSomeScarletKeyCard :: (forall a. ScarletKeyCard a -> b) -> SomeScarletKeyCard -> b
liftSomeScarletKeyCard f (SomeScarletKeyCard a) = f a

someScarletKeyCardCode :: SomeScarletKeyCard -> CardCode
someScarletKeyCardCode = liftSomeScarletKeyCard cbCardCode

makeLensesWith suffixedFields ''ScarletKeyAttrs

mconcat
  [ deriveJSON defaultOptions ''Stability
  , deriveToJSON (aesonOptions $ Just "key") ''ScarletKeyAttrs
  ]

instance FromJSON ScarletKeyAttrs where
  parseJSON = withObject "ScarletKeyAttrs" \o -> do
    keyId <- o .: "id"
    keyCardId <- o .: "cardId"
    keyPlacement <- o .: "placement"
    keyStability <- o .: "stability"
    keyBearer <- o .: "bearer"
    pure ScarletKeyAttrs {..}
