{-# LANGUAGE TemplateHaskell #-}

module Arkham.EnemyLocation.Types where

import Arkham.Calculation
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Cost (Cost (Free))
import Arkham.DamageEffect
import Arkham.Direction
import Arkham.EnemyLocation.Cards
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.Location.Base
import Arkham.Location.Grid
import Arkham.LocationSymbol (LocationSymbol (NoSymbol))
import Arkham.Matcher
import Arkham.Name
import Arkham.Placement
import Arkham.Prelude
import Arkham.SkillType (SkillType (SkillIntellect))
import Arkham.Source
import Arkham.Target
import Arkham.Token
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
  { enemyLocationBase :: LocationAttrs
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

-- | Construct the location-side base of an enemy-location.
-- Fills in all fixed values that never apply to enemy-locations (symbol,
-- revealClues, etc.) and accepts only the fields that vary per-instance.
mkEnemyLocationBase :: LocationId -> CardId -> CardCode -> Text -> LocationAttrs
mkEnemyLocationBase lid cardId cardCode label =
  LocationAttrs
    { locationId = lid
    , locationCardCode = cardCode
    , locationCardId = cardId
    , locationLabel = label
    , locationRevealClues = Static 0
    , locationTokens = mempty
    , locationShroud = Nothing
    , locationRevealed = True
    , locationSymbol = NoSymbol
    , locationRevealedSymbol = NoSymbol
    , locationConnectedMatchers = []
    , locationRevealedConnectedMatchers = []
    , locationDirections = mempty
    , locationConnectsTo = mempty
    , locationCardsUnderneath = []
    , locationCostToEnterUnrevealed = Free
    , locationCanBeFlipped = False
    , locationInvestigateSkill = SkillIntellect
    , locationPlacement = Nothing
    , locationKeys = mempty
    , locationSeals = mempty
    , locationSealedChaosTokens = []
    , locationFloodLevel = Nothing
    , locationBrazier = Nothing
    , locationBreaches = Nothing
    , locationWithoutClues = True
    , locationMeta = Null
    , locationGlobalMeta = mempty
    , locationPosition = Nothing
    , locationBeingRemoved = False
    , locationConcealedCards = []
    , locationOutOfGame = False
    }

instance AsId EnemyLocationAttrs where
  type IdOf EnemyLocationAttrs = LocationId
  asId = locationId . enemyLocationBase

instance HasField "id" EnemyLocationAttrs LocationId where
  getField = locationId . enemyLocationBase

instance HasField "cardId" EnemyLocationAttrs CardId where
  getField = locationCardId . enemyLocationBase

instance HasField "cardCode" EnemyLocationAttrs CardCode where
  getField = locationCardCode . enemyLocationBase

instance HasField "label" EnemyLocationAttrs Text where
  getField = locationLabel . enemyLocationBase

instance HasField "meta" EnemyLocationAttrs Value where
  getField = locationMeta . enemyLocationBase

instance HasField "tokens" EnemyLocationAttrs Tokens where
  getField = locationTokens . enemyLocationBase

instance HasField "token" EnemyLocationAttrs (Token -> Int) where
  getField a tType = countTokens tType a.tokens

instance HasField "clues" EnemyLocationAttrs Int where
  getField = locationClues . enemyLocationBase

instance HasField "placement" EnemyLocationAttrs (Maybe Placement) where
  getField = locationPlacement . enemyLocationBase

instance HasField "position" EnemyLocationAttrs (Maybe Pos) where
  getField = locationPosition . enemyLocationBase

instance HasField "shroud" EnemyLocationAttrs (Maybe GameValue) where
  getField = locationShroud . enemyLocationBase

instance HasField "directions" EnemyLocationAttrs (Map Direction [LocationId]) where
  getField = locationDirections . enemyLocationBase

instance HasField "connectedMatchers" EnemyLocationAttrs [LocationMatcher] where
  getField = locationConnectedMatchers . enemyLocationBase

instance HasField "revealedConnectedMatchers" EnemyLocationAttrs [LocationMatcher] where
  getField = locationRevealedConnectedMatchers . enemyLocationBase

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

instance HasField "healthDamage" EnemyLocationAttrs Int where
  getField = enemyLocationHealthDamage

instance HasField "sanityDamage" EnemyLocationAttrs Int where
  getField = enemyLocationSanityDamage

instance HasField "ability" EnemyLocationAttrs (Int -> Source) where
  getField = toAbilitySource

instance Entity EnemyLocationAttrs where
  type EntityId EnemyLocationAttrs = LocationId
  type EntityAttrs EnemyLocationAttrs = EnemyLocationAttrs
  toId = locationId . enemyLocationBase
  toAttrs = id
  overAttrs f = f

instance Named EnemyLocationAttrs where
  toName = toName . toCardDef

instance HasCardCode EnemyLocationAttrs where
  toCardCode = locationCardCode . enemyLocationBase

instance HasCardDef EnemyLocationAttrs where
  toCardDef e = case lookup (toCardCode e) allEnemyLocationCards of
    Just def -> def
    Nothing -> error $ "missing card def for enemy-location " <> show (toCardCode e)

instance IsCard EnemyLocationAttrs where
  toCardId = locationCardId . enemyLocationBase
  toCard e =
    EncounterCard
      MkEncounterCard
        { ecId = toCardId e
        , ecCardCode = toCardCode e
        , ecOriginalCardCode = enemyLocationOriginalCardCode e
        , ecIsFlipped = Just True
        , ecAddedPeril = False
        , ecOwner = Nothing
        , ecFacedown = Nothing
        }
  toCardOwner = const Nothing

instance Targetable EnemyLocationAttrs where
  toTarget = LocationTarget . toId
  isTarget a (LocationTarget lid) = toId a == lid
  isTarget a (CardCodeTarget cardCode) = toCardCode a == cardCode
  isTarget a (CardIdTarget cardId) = toCardId a == cardId
  isTarget _ _ = False

instance Sourceable EnemyLocationAttrs where
  toSource = LocationSource . toId
  isSource a (LocationSource lid) = toId a == lid
  isSource a (CardCodeSource cardCode) = toCardCode a == cardCode
  isSource a (AbilitySource source _) = isSource a source
  isSource a (UseAbilitySource _ source _) = isSource a source
  isSource _ _ = False

-- | The damage on this enemy-location
enemyLocationDamage :: EnemyLocationAttrs -> Int
enemyLocationDamage = countTokens Damage . locationTokens . enemyLocationBase

-- | The coerced EnemyId for fight/evade targeting
enemyLocationAsEnemyId :: EnemyLocationId -> EnemyId
enemyLocationAsEnemyId (EnemyLocationId lid) = EnemyId $ coerce $ unLocationId lid

instance ToJSON EnemyLocationAttrs where
  toJSON a =
    let base = enemyLocationBase a
     in object
          [ "id" .= locationId base
          , "cardCode" .= locationCardCode base
          , "cardId" .= locationCardId base
          , "originalCardCode" .= enemyLocationOriginalCardCode a
          , "label" .= locationLabel base
          , "tokens" .= locationTokens base
          , "shroud" .= locationShroud base
          , "connectedMatchers" .= locationConnectedMatchers base
          , "revealedConnectedMatchers" .= locationRevealedConnectedMatchers base
          , "position" .= locationPosition base
          , "placement" .= locationPlacement base
          , "meta" .= locationMeta base
          , "directions" .= locationDirections base
          , "connectsTo" .= locationConnectsTo base
          , "fight" .= enemyLocationFight a
          , "health" .= enemyLocationHealth a
          , "evade" .= enemyLocationEvade a
          , "healthDamage" .= enemyLocationHealthDamage a
          , "sanityDamage" .= enemyLocationSanityDamage a
          , "assignedDamage" .= enemyLocationAssignedDamage a
          , "defeated" .= enemyLocationDefeated a
          , "exhausted" .= enemyLocationExhausted a
          ]

instance FromJSON EnemyLocationAttrs where
  parseJSON = withObject "EnemyLocationAttrs" \o -> do
    lid <- o .: "id"
    cardId <- o .: "cardId"
    cardCode <- o .: "cardCode"
    originalCardCode <- o .: "originalCardCode"
    label <- o .: "label"
    tokens <- o .: "tokens"
    shroud <- o .:? "shroud"
    connectedMatchers <- o .: "connectedMatchers"
    revealedConnectedMatchers <- o .: "revealedConnectedMatchers"
    position <- o .:? "position"
    placement <- o .:? "placement"
    meta <- o .:? "meta" .!= Null
    directions <- o .: "directions"
    connectsTo <- o .: "connectsTo"
    fight <- o .:? "fight"
    health <- o .:? "health"
    evade <- o .:? "evade"
    healthDamage <- o .: "healthDamage"
    sanityDamage <- o .: "sanityDamage"
    assignedDamage <- o .:? "assignedDamage" .!= mempty
    defeated <- o .:? "defeated" .!= False
    exhausted <- o .:? "exhausted" .!= False
    pure
      EnemyLocationAttrs
        { enemyLocationBase =
            (mkEnemyLocationBase lid cardId cardCode label)
              { locationTokens = tokens
              , locationShroud = shroud
              , locationConnectedMatchers = connectedMatchers
              , locationRevealedConnectedMatchers = revealedConnectedMatchers
              , locationPosition = position
              , locationPlacement = placement
              , locationMeta = meta
              , locationDirections = directions
              , locationConnectsTo = connectsTo
              }
        , enemyLocationFight = fight
        , enemyLocationHealth = health
        , enemyLocationEvade = evade
        , enemyLocationHealthDamage = healthDamage
        , enemyLocationSanityDamage = sanityDamage
        , enemyLocationAssignedDamage = assignedDamage
        , enemyLocationDefeated = defeated
        , enemyLocationExhausted = exhausted
        , enemyLocationOriginalCardCode = originalCardCode
        }

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
  getField (EnemyLocation e) = attr (locationId . enemyLocationBase) e

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
        f . g $
          EnemyLocationAttrs
            { enemyLocationBase =
                mkEnemyLocationBase
                  lid
                  cardId
                  (toCardCode cardDef)
                  (T.toLower . T.replace " " "_" $ display cardDef.name)
            , enemyLocationFight = Just $ Fixed fight
            , enemyLocationHealth = Just $ GameValueCalculation health
            , enemyLocationEvade = Just $ Fixed evade
            , enemyLocationHealthDamage = healthDamage
            , enemyLocationSanityDamage = sanityDamage
            , enemyLocationAssignedDamage = mempty
            , enemyLocationDefeated = False
            , enemyLocationExhausted = False
            , enemyLocationOriginalCardCode = toCardCode cardDef
            }
    }
