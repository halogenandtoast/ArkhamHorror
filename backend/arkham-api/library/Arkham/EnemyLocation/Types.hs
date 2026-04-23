{-# LANGUAGE TemplateHaskell #-}

module Arkham.EnemyLocation.Types where

import Arkham.Calculation
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.DamageEffect
import Arkham.Direction
import Arkham.EnemyLocation.Cards
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Name
import Arkham.Placement
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Data.Aeson.TH
import Data.Data
import Data.Text qualified as T
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
  , EntityId a ~ LocationId
  , EntityAttrs a ~ EnemyLocationAttrs
  , RunType a ~ a
  ) =>
  IsEnemyLocation a

type EnemyLocationCard a = CardBuilder LocationId a

{- | An enemy-location is both an enemy and a location.
Investigators may fight, evade, and investigate enemy-locations.
Enemy-locations cannot be moved by card effects.
-}
data EnemyLocationAttrs = EnemyLocationAttrs
  { enemyLocationId :: LocationId
  , enemyLocationCardCode :: CardCode
  , enemyLocationCardId :: CardId
  , enemyLocationLabel :: Text
  , -- Location properties
    enemyLocationTokens :: Tokens
  , enemyLocationShroud :: Maybe GameValue
  , enemyLocationConnectedMatchers :: [LocationMatcher]
  , enemyLocationRevealedConnectedMatchers :: [LocationMatcher]
  , enemyLocationPosition :: Maybe Pos
  , enemyLocationPlacement :: Maybe Placement
  , enemyLocationMeta :: Value
  , enemyLocationDirections :: Map Direction [LocationId]
  , enemyLocationConnectsTo :: Set Direction
  , -- Enemy properties
    enemyLocationFight :: Maybe GameCalculation
  , enemyLocationHealth :: Maybe GameCalculation
  , enemyLocationEvade :: Maybe GameCalculation
  , enemyLocationHealthDamage :: Int
  , enemyLocationSanityDamage :: Int
  , enemyLocationAssignedDamage :: Map Source DamageAssignment
  , enemyLocationDefeated :: Bool
  , enemyLocationExhausted :: Bool
  , enemyLocationOriginalCardCode :: CardCode
  }
  deriving stock (Show, Eq)

instance AsId EnemyLocationAttrs where
  type IdOf EnemyLocationAttrs = LocationId
  asId = enemyLocationId

instance HasField "id" EnemyLocationAttrs LocationId where
  getField = enemyLocationId

instance HasField "cardId" EnemyLocationAttrs CardId where
  getField = enemyLocationCardId

instance HasField "cardCode" EnemyLocationAttrs CardCode where
  getField = enemyLocationCardCode

instance HasField "label" EnemyLocationAttrs Text where
  getField = enemyLocationLabel

instance HasField "meta" EnemyLocationAttrs Value where
  getField = enemyLocationMeta

instance HasField "tokens" EnemyLocationAttrs Tokens where
  getField = enemyLocationTokens

instance HasField "token" EnemyLocationAttrs (Token -> Int) where
  getField a tType = countTokens tType a.tokens

instance HasField "clues" EnemyLocationAttrs Int where
  getField = countTokens Clue . enemyLocationTokens

instance HasField "placement" EnemyLocationAttrs (Maybe Placement) where
  getField = enemyLocationPlacement

instance HasField "position" EnemyLocationAttrs (Maybe Pos) where
  getField = enemyLocationPosition

instance HasField "defeated" EnemyLocationAttrs Bool where
  getField = enemyLocationDefeated

instance HasField "exhausted" EnemyLocationAttrs Bool where
  getField = enemyLocationExhausted

instance HasField "fight" EnemyLocationAttrs (Maybe GameCalculation) where
  getField = enemyLocationFight

instance HasField "health" EnemyLocationAttrs (Maybe GameCalculation) where
  getField = enemyLocationHealth

instance HasField "evade" EnemyLocationAttrs (Maybe GameCalculation) where
  getField = enemyLocationEvade

instance HasField "ability" EnemyLocationAttrs (Int -> Source) where
  getField = toAbilitySource

instance Entity EnemyLocationAttrs where
  type EntityId EnemyLocationAttrs = LocationId
  type EntityAttrs EnemyLocationAttrs = EnemyLocationAttrs
  toId = enemyLocationId
  toAttrs = id
  overAttrs f = f

instance Named EnemyLocationAttrs where
  toName = toName . toCardDef

instance HasCardCode EnemyLocationAttrs where
  toCardCode = enemyLocationCardCode

instance HasCardDef EnemyLocationAttrs where
  toCardDef e = case lookup (enemyLocationCardCode e) allEnemyLocationCards of
    Just def -> def
    Nothing -> error $ "missing card def for enemy-location " <> show (enemyLocationCardCode e)

instance IsCard EnemyLocationAttrs where
  toCardId = enemyLocationCardId
  toCard e =
    EncounterCard
      MkEncounterCard
        { ecId = enemyLocationCardId e
        , ecCardCode = enemyLocationCardCode e
        , ecOriginalCardCode = enemyLocationOriginalCardCode e
        , ecIsFlipped = Nothing
        , ecAddedPeril = False
        , ecOwner = Nothing
        }
  toCardOwner = const Nothing

instance Targetable EnemyLocationAttrs where
  toTarget = LocationTarget . toId
  isTarget EnemyLocationAttrs {enemyLocationId} (LocationTarget lid) = enemyLocationId == lid
  isTarget attrs (CardCodeTarget cardCode) = toCardCode attrs == cardCode
  isTarget attrs (CardIdTarget cardId) = toCardId attrs == cardId
  isTarget _ _ = False

instance Sourceable EnemyLocationAttrs where
  toSource = LocationSource . toId
  isSource EnemyLocationAttrs {enemyLocationId} (LocationSource lid) = enemyLocationId == lid
  isSource attrs (CardCodeSource cardCode) = toCardCode attrs == cardCode
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs (UseAbilitySource _ source _) = isSource attrs source
  isSource _ _ = False

-- | The damage on this enemy-location
enemyLocationDamage :: EnemyLocationAttrs -> Int
enemyLocationDamage = countTokens Damage . enemyLocationTokens

-- | The coerced EnemyId for fight/evade targeting
enemyLocationAsEnemyId :: EnemyLocationId -> EnemyId
enemyLocationAsEnemyId (EnemyLocationId lid) = EnemyId $ coerce $ unLocationId lid

$(deriveToJSON (aesonOptions $ Just "enemyLocation") ''EnemyLocationAttrs)

instance FromJSON EnemyLocationAttrs where
  parseJSON = withObject "EnemyLocationAttrs" \o -> do
    enemyLocationId <- o .: "id"
    enemyLocationCardId <- o .: "cardId"
    enemyLocationCardCode <- o .: "cardCode"
    enemyLocationOriginalCardCode <- o .: "originalCardCode"
    enemyLocationLabel <- o .: "label"
    enemyLocationTokens <- o .: "tokens"
    enemyLocationShroud <- o .:? "shroud"
    enemyLocationConnectedMatchers <- o .: "connectedMatchers"
    enemyLocationRevealedConnectedMatchers <- o .: "revealedConnectedMatchers"
    enemyLocationPosition <- o .:? "position"
    enemyLocationPlacement <- o .:? "placement"
    enemyLocationMeta <- o .:? "meta" .!= Null
    enemyLocationFight <- o .:? "fight"
    enemyLocationHealth <- o .:? "health"
    enemyLocationEvade <- o .:? "evade"
    enemyLocationHealthDamage <- o .: "healthDamage"
    enemyLocationSanityDamage <- o .: "sanityDamage"
    enemyLocationAssignedDamage <- o .:? "assignedDamage" .!= mempty
    enemyLocationDefeated <- o .:? "defeated" .!= False
    enemyLocationExhausted <- o .:? "exhausted" .!= False
    enemyLocationDirections <- o .: "directions"
    enemyLocationConnectsTo <- o .: "connectsTo"
    pure EnemyLocationAttrs {..}

data EnemyLocation = forall a. IsEnemyLocation a => EnemyLocation a

instance Show EnemyLocation where
  show (EnemyLocation a) = show a

instance Eq EnemyLocation where
  EnemyLocation (a :: a) == EnemyLocation (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Data EnemyLocation where
  gunfold _ _ _ = error "gunfold(EnemyLocation)"
  toConstr _ = error "toConstr(EnemyLocation)"
  dataTypeOf _ = error "dataTypeOf(EnemyLocation)"

instance ToJSON EnemyLocation where
  toJSON (EnemyLocation a) = toJSON a

instance AsId EnemyLocation where
  type IdOf EnemyLocation = LocationId
  asId = toId

instance HasField "id" EnemyLocation LocationId where
  getField (EnemyLocation e) = attr enemyLocationId e

instance HasField "placement" EnemyLocation (Maybe Placement) where
  getField = (.placement) . toAttrs

instance HasField "position" EnemyLocation (Maybe Pos) where
  getField = (.position) . toAttrs

instance HasField "defeated" EnemyLocation Bool where
  getField = (.defeated) . toAttrs

instance HasField "exhausted" EnemyLocation Bool where
  getField = (.exhausted) . toAttrs

instance HasCardCode EnemyLocation where
  toCardCode (EnemyLocation a) = toCardCode (toAttrs a)

instance HasCardDef EnemyLocation where
  toCardDef (EnemyLocation a) = toCardDef (toAttrs a)

instance IsCard EnemyLocation where
  toCard (EnemyLocation a) = toCard (toAttrs a)
  toCardId (EnemyLocation a) = toCardId (toAttrs a)
  toCardOwner (EnemyLocation a) = toCardOwner (toAttrs a)

instance Named EnemyLocation where
  toName (EnemyLocation a) = toName (toAttrs a)

instance HasAbilities EnemyLocation where
  getAbilities (EnemyLocation a) = getAbilities a

instance HasModifiersFor EnemyLocation where
  getModifiersFor (EnemyLocation a) = getModifiersFor a

instance Entity EnemyLocation where
  type EntityId EnemyLocation = LocationId
  type EntityAttrs EnemyLocation = EnemyLocationAttrs
  toId = toId . toAttrs
  toAttrs (EnemyLocation a) = toAttrs a
  overAttrs f (EnemyLocation a) = EnemyLocation $ overAttrs f a

instance Targetable EnemyLocation where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable EnemyLocation where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

data SomeEnemyLocationCard = forall a. IsEnemyLocation a => SomeEnemyLocationCard (EnemyLocationCard a)

someEnemyLocationCardCode :: SomeEnemyLocationCard -> CardCode
someEnemyLocationCardCode (SomeEnemyLocationCard a) = toCardCode a

makeLensesWith (suffixedWithFields "enemyLocation") ''EnemyLocationAttrs

-- | Create an enemy-location from attrs given fight/health/evade stats and damage stats
enemyLocation
  :: (EnemyLocationAttrs -> a)
  -> CardDef
  -> (Int, GameValue, Int)
  -> (Int, Int)
  -> CardBuilder LocationId a
enemyLocation f cardDef stats damageStats = enemyLocationWith f cardDef stats damageStats id

enemyLocationWith
  :: (EnemyLocationAttrs -> a)
  -> CardDef
  -> (Int, GameValue, Int)
  -> (Int, Int)
  -> (EnemyLocationAttrs -> EnemyLocationAttrs)
  -> CardBuilder LocationId a
enemyLocationWith f cardDef (fight, health, evade) (healthDamage, sanityDamage) g =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId lid ->
        f
          . g
          $ EnemyLocationAttrs
            { enemyLocationId = lid
            , enemyLocationCardId = cardId
            , enemyLocationCardCode = toCardCode cardDef
            , enemyLocationOriginalCardCode = toCardCode cardDef
            , enemyLocationLabel = T.toLower . T.replace " " "_" $ display cardDef.name
            , enemyLocationTokens = mempty
            , enemyLocationShroud = Nothing
            , enemyLocationConnectedMatchers = []
            , enemyLocationRevealedConnectedMatchers = []
            , enemyLocationPosition = Nothing
            , enemyLocationPlacement = Nothing
            , enemyLocationMeta = Null
            , enemyLocationFight = Just $ Fixed fight
            , enemyLocationHealth = Just $ GameValueCalculation health
            , enemyLocationEvade = Just $ Fixed evade
            , enemyLocationHealthDamage = healthDamage
            , enemyLocationSanityDamage = sanityDamage
            , enemyLocationAssignedDamage = mempty
            , enemyLocationDefeated = False
            , enemyLocationExhausted = False
            , enemyLocationDirections = mempty
            , enemyLocationConnectsTo = mempty
            }
    }
