{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Location.Types
  ( module Arkham.Location.Types
  , module X
  , Field(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import Arkham.Keyword
import Arkham.Label qualified as L
import Arkham.Location.Base as X
import Arkham.Location.Cards
import Arkham.LocationSymbol
import Arkham.Matcher ( LocationMatcher (..) )
import Arkham.Name
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Trait ( Trait )
import Data.Text qualified as T
import Data.Typeable

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
  , EntityAttrs a ~ LocationAttrs
  ) => IsLocation a where
    toLocation :: a -> Location
    toLocation = Location

type LocationCard a = CardBuilder LocationId a

data instance Field Location :: Type -> Type where
  LocationClues :: Field Location Int
  LocationResources :: Field Location Int
  LocationHorror :: Field Location Int
  LocationDoom :: Field Location Int
  LocationShroud :: Field Location Int
  LocationTraits :: Field Location (HashSet Trait)
  LocationKeywords :: Field Location (HashSet Keyword)
  LocationUnrevealedName :: Field Location Name
  LocationName :: Field Location Name
  LocationConnectedMatchers :: Field Location [LocationMatcher]
  LocationRevealedConnectedMatchers :: Field Location [LocationMatcher]
  LocationRevealed :: Field Location Bool
  LocationConnectsTo :: Field Location (HashSet Direction)
  LocationCardsUnderneath :: Field Location [Card]
  LocationConnectedLocations :: Field Location (HashSet LocationId)
  LocationInvestigators :: Field Location (HashSet InvestigatorId)
  LocationEnemies :: Field Location (HashSet EnemyId)
  LocationAssets :: Field Location (HashSet AssetId)
  LocationEvents :: Field Location (HashSet EventId)
  LocationTreacheries :: Field Location (HashSet TreacheryId)
  LocationInvestigateSkill :: Field Location SkillType
  LocationInFrontOf :: Field Location (Maybe InvestigatorId)
  LocationCardId :: Field Location CardId
  LocationLabel :: Field Location Text
  -- virtual
  LocationCardDef :: Field Location CardDef
  LocationCard :: Field Location Card
  LocationAbilities :: Field Location [Ability]
  LocationPrintedSymbol :: Field Location LocationSymbol
  LocationVengeance :: Field Location (Maybe Int)

deriving stock instance Show (Field Location typ)

instance ToJSON (Field Location typ) where
  toJSON = toJSON . show

instance FromJSON (SomeField Location) where
  parseJSON = withText "Field Location" $ \case
    "LocationInFrontOf" -> pure $ SomeField LocationInFrontOf
    "LocationInvestigateSkill" -> pure $ SomeField LocationInvestigateSkill
    "LocationClues" -> pure $ SomeField LocationClues
    "LocationResources" -> pure $ SomeField LocationResources
    "LocationHorror" -> pure $ SomeField LocationHorror
    "LocationDoom" -> pure $ SomeField LocationDoom
    "LocationShroud" -> pure $ SomeField LocationShroud
    "LocationTraits" -> pure $ SomeField LocationTraits
    "LocationKeywords" -> pure $ SomeField LocationKeywords
    "LocationUnrevealedName" -> pure $ SomeField LocationUnrevealedName
    "LocationName" -> pure $ SomeField LocationName
    "LocationConnectedMatchers" -> pure $ SomeField LocationConnectedMatchers
    "LocationRevealedConnectedMatchers" ->
      pure $ SomeField LocationRevealedConnectedMatchers
    "LocationRevealed" -> pure $ SomeField LocationRevealed
    "LocationConnectsTo" -> pure $ SomeField LocationConnectsTo
    "LocationCardsUnderneath" -> pure $ SomeField LocationCardsUnderneath
    "LocationConnectedLocations" -> pure $ SomeField LocationConnectedLocations
    "LocationInvestigators" -> pure $ SomeField LocationInvestigators
    "LocationEnemies" -> pure $ SomeField LocationEnemies
    "LocationAssets" -> pure $ SomeField LocationAssets
    "LocationEvents" -> pure $ SomeField LocationEvents
    "LocationTreacheries" -> pure $ SomeField LocationTreacheries
    "LocationCardDef" -> pure $ SomeField LocationCardDef
    "LocationCard" -> pure $ SomeField LocationCard
    "LocationAbilities" -> pure $ SomeField LocationAbilities
    "LocationPrintedSymbol" -> pure $ SomeField LocationPrintedSymbol
    "LocationVengeance" -> pure $ SomeField LocationVengeance
    _ -> error "no such field"

instance Entity LocationAttrs where
  type EntityId LocationAttrs = LocationId
  type EntityAttrs LocationAttrs = LocationAttrs
  toId = locationId
  toAttrs = id
  overAttrs f = f

instance Targetable LocationAttrs where
  toTarget = LocationTarget . toId
  isTarget LocationAttrs { locationId } (LocationTarget lid) =
    locationId == lid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget attrs (BothTarget t1 t2) = isTarget attrs t1 || isTarget attrs t2
  isTarget _ _ = False

instance Sourceable LocationAttrs where
  toSource = LocationSource . toId
  isSource LocationAttrs { locationId } (LocationSource lid) =
    locationId == lid
  isSource _ _ = False


instance HasCardCode LocationAttrs where
  toCardCode = locationCardCode

instance HasCardDef LocationAttrs where
  toCardDef a = case lookup (locationCardCode a) (allLocationCards <> allSpecialLocationCards) of
    Just def -> def
    Nothing ->
      error $ "missing card def for location " <> show (locationCardCode a)

unrevealed :: LocationAttrs -> Bool
unrevealed = not . locationRevealed

revealed :: LocationAttrs -> Bool
revealed = locationRevealed

location
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue
  -> CardBuilder LocationId a
location f def shroud' revealClues = locationWith f def shroud' revealClues id

locationWith
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue
  -> (LocationAttrs -> LocationAttrs)
  -> CardBuilder LocationId a
locationWith f def shroud' revealClues g = CardBuilder
  { cbCardCode = cdCardCode def
  , cbCardBuilder = \cardId lid -> f . g $ LocationAttrs
    { locationId = lid
    , locationCardCode = toCardCode def
    , locationCardId = cardId
    , locationLabel = nameToLabel (cdName def)
    , locationRevealClues = revealClues
    , locationClues = 0
    , locationHorror = 0
    , locationDoom = 0
    , locationResources = 0
    , locationShroud = shroud'
    , locationRevealed = not (cdDoubleSided def)
    , locationInvestigators = mempty
    , locationEnemies = mempty
    , locationSymbol = fromJustNote
      "missing location symbol"
      (cdLocationSymbol def)
    , locationRevealedSymbol = fromJustNote
      "missing revealed location symbol"
      (cdLocationRevealedSymbol def)
    , locationConnectedMatchers = map
      LocationWithSymbol
      (cdLocationConnections def)
    , locationRevealedConnectedMatchers = map
      LocationWithSymbol
      (cdLocationRevealedConnections def)
    , locationTreacheries = mempty
    , locationEvents = mempty
    , locationAssets = mempty
    , locationDirections = mempty
    , locationConnectsTo = mempty
    , locationCardsUnderneath = mempty
    , locationCostToEnterUnrevealed = ActionCost 1
    , locationInvestigateSkill = SkillIntellect
    , locationCanBeFlipped = False
    , locationInFrontOf = Nothing
    , locationWithoutClues = False
    }
  }

locationResignAction :: LocationAttrs -> Ability
locationResignAction attrs = toLocationAbility
  attrs
  (mkAbility attrs 99 $ ActionAbility (Just Action.Resign) (ActionCost 1))

toLocationAbility :: LocationAttrs -> Ability -> Ability
toLocationAbility attrs ability = ability
  { abilityCriteria = Just
    (fromMaybe mempty (abilityCriteria ability)
    <> OnLocation (LocationWithId $ toId attrs)
    )
  }

locationAbility :: Ability -> Ability
locationAbility ability = case abilitySource ability of
  LocationSource lid -> ability
    { abilityCriteria = Just
      (fromMaybe mempty (abilityCriteria ability)
      <> OnLocation (LocationWithId lid)
      )
    }
  _ -> ability

on :: InvestigatorId -> LocationAttrs -> Bool
on iid LocationAttrs { locationInvestigators } =
  iid `member` locationInvestigators

data Location = forall a . IsLocation a => Location a

instance Eq Location where
  Location (a :: a) == Location (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Location where
  show (Location a) = show a

instance ToJSON Location where
  toJSON (Location a) = toJSON a

toLocationSymbol :: Location -> LocationSymbol
toLocationSymbol l = if locationRevealed attrs
  then locationRevealedSymbol attrs
  else locationSymbol attrs
  where attrs = toAttrs l

toLocationLabel :: Location -> L.Label
toLocationLabel = L.Label . locationLabel . toAttrs

instance HasCardCode Location where
  toCardCode = toCardCode . toAttrs

instance HasAbilities Location where
  getAbilities (Location a) = getAbilities a

instance HasModifiersFor Location where
  getModifiersFor target (Location a) = getModifiersFor target a

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs
  toId = toId . toAttrs
  toAttrs (Location l) = toAttrs l
  overAttrs f (Location a) = Location $ overAttrs f a

instance Named Location where
  toName = toName . toAttrs

instance Named (Unrevealed Location) where
  toName (Unrevealed l) = toName . Unrevealed $ toAttrs l

instance Targetable Location where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Location where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

isEmptyLocation :: Location -> Bool
isEmptyLocation =
  and . sequence [noInvestigatorsAtLocation, noEnemiesAtLocation]

noInvestigatorsAtLocation :: Location -> Bool
noInvestigatorsAtLocation l = null investigators'
  where investigators' = locationInvestigators $ toAttrs l

noEnemiesAtLocation :: Location -> Bool
noEnemiesAtLocation l = null enemies'
  where enemies' = locationEnemies $ toAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . toAttrs

data SomeLocationCard where
  SomeLocationCard :: IsLocation a => LocationCard a -> SomeLocationCard

someLocationCardCode :: SomeLocationCard -> CardCode
someLocationCardCode (SomeLocationCard a) = cbCardCode a

instance Named LocationAttrs where
  toName l = if locationRevealed l
    then fromMaybe baseName (cdRevealedName $ toCardDef l)
    else baseName
    where baseName = toName (toCardDef l)

instance Named (Unrevealed LocationAttrs) where
  toName (Unrevealed l) = toName (toCardDef l)

instance IsCard LocationAttrs where
  toCardId = locationCardId
  toCardOwner = const Nothing

makeLensesWith suffixedFields ''LocationAttrs

symbolLabel
  :: (Entity a, EntityAttrs a ~ LocationAttrs)
  => CardBuilder LocationId a
  -> CardBuilder LocationId a
symbolLabel = fmap
  (overAttrs
    (\attrs -> attrs & labelL .~ (T.toLower . tshow $ locationSymbol attrs))
  )
