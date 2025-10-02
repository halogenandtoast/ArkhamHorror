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
import Arkham.Json hiding (Key)
import Arkham.Message (Is (..))
import Arkham.Name
import Arkham.Placement
import Arkham.Prelude hiding (Key)
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
  , EntityId a ~ KeyId
  , EntityAttrs a ~ KeyAttrs
  , RunType a ~ a
  ) =>
  IsKey a

type KeyCard a = CardBuilder (Target, KeyId) a

data instance Field Key :: Type -> Type where
  KeyCard :: Field Key Card
  KeyClues :: Field Key Int
  KeyPlacement :: Field Key Placement
  KeyOtherSide :: Field Key (Maybe Target)
  KeyCardsUnderneath :: Field Key [Card]

data Stability = Stable | Unstable
  deriving stock (Show, Eq, Data)

data KeyAttrs = KeyAttrs
  { keyId :: KeyId
  , keyCardId :: CardId
  , keyPlacement :: Placement
  , keyStability :: Stability
  , keyBearer :: Target
  }
  deriving stock (Show, Eq)

instance Is KeyAttrs KeyId where
  is = (==) . toId
  {-# INLINE is #-}

instance AsId KeyAttrs where
  type IdOf KeyAttrs = KeyId
  asId = keyId

instance HasField "cardId" KeyAttrs CardId where
  getField = keyCardId

instance HasField "id" KeyAttrs KeyId where
  getField = keyId

instance HasField "ability" KeyAttrs (Int -> Source) where
  getField = toAbilitySource

instance HasField "placement" KeyAttrs Placement where
  getField = keyPlacement

key :: (KeyAttrs -> a) -> CardDef -> CardBuilder (Target, KeyId) a
key f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (bearer, kid) ->
        f
          $ KeyAttrs
            { keyId = kid
            , keyCardId = cardId
            , keyPlacement = Unplaced
            , keyStability = case bearer of
                InvestigatorTarget _ -> Stable
                _ -> Unstable
            , keyBearer = bearer
            }
    }

instance HasCardDef KeyAttrs where
  toCardDef e = case lookup (unKeyId $ keyId e) allKeyCards of
    Just def -> def
    Nothing -> error $ "missing card def for key " <> show (unKeyId $ keyId e)

instance HasCardCode Key where
  toCardCode (Key a) = toCardCode (toAttrs a)
  {-# INLINE toCardCode #-}

instance Entity KeyAttrs where
  type EntityId KeyAttrs = KeyId
  type EntityAttrs KeyAttrs = KeyAttrs
  toId = keyId
  toAttrs = id
  overAttrs f = f

instance Named Key where
  toName = toName . toAttrs

instance Named KeyAttrs where
  toName = toName . toCardDef

instance Targetable KeyAttrs where
  toTarget = ScarletKeyTarget . toId
  isTarget KeyAttrs {keyId} (ScarletKeyTarget sid) = keyId == sid
  isTarget _ _ = False

instance Sourceable KeyAttrs where
  toSource = ScarletKeySource . toId
  isSource KeyAttrs {keyId} (ScarletKeySource sid) = keyId == sid
  isSource _ _ = False

data Key = forall a. IsKey a => Key a

instance Data Key where
  gunfold _ _ _ = error "gunfold(Key)"
  toConstr _ = error "toConstr(Key)"
  dataTypeOf _ = error "dataTypeOf(Key)"

instance Eq Key where
  (Key (a :: a)) == (Key (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Key where
  show (Key a) = show a

instance ToJSON Key where
  toJSON (Key a) = toJSON a

instance HasAbilities Key where
  getAbilities (Key a) = getAbilities a

instance HasModifiersFor Key where
  getModifiersFor (Key a) = getModifiersFor a

instance Entity Key where
  type EntityId Key = KeyId
  type EntityAttrs Key = KeyAttrs
  toId = toId . toAttrs
  toAttrs (Key a) = toAttrs a
  overAttrs f (Key a) = Key $ overAttrs f a

instance Targetable Key where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Key where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardCode KeyAttrs where
  toCardCode = unKeyId . keyId

instance IsCard KeyAttrs where
  toCardId = keyCardId
  toCard a = lookupCard (unKeyId $ keyId a) (toCardId a)
  toCardOwner _ = Nothing

data SomeKeyCard = forall a. IsKey a => SomeKeyCard (KeyCard a)

liftSomeKeyCard :: (forall a. KeyCard a -> b) -> SomeKeyCard -> b
liftSomeKeyCard f (SomeKeyCard a) = f a

someKeyCardCode :: SomeKeyCard -> CardCode
someKeyCardCode = liftSomeKeyCard cbCardCode

makeLensesWith suffixedFields ''KeyAttrs

mconcat
  [ deriveJSON defaultOptions ''Stability
  , deriveToJSON (aesonOptions $ Just "key") ''KeyAttrs
  ]

instance FromJSON KeyAttrs where
  parseJSON = withObject "KeyAttrs" \o -> do
    keyId <- o .: "id"
    keyCardId <- o .: "cardId"
    keyPlacement <- o .: "placement"
    keyStability <- o .: "stability"
    keyBearer <- o .: "bearer"
    pure KeyAttrs {..}
