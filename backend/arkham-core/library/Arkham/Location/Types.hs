{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Location.Types (
  module Arkham.Location.Types,
  module X,
  Field (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Direction
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import Arkham.Keyword
import Arkham.Label qualified as L
import Arkham.Location.Base as X
import Arkham.Location.Brazier
import Arkham.Location.BreachStatus
import Arkham.Location.Cards
import Arkham.LocationSymbol
import Arkham.Matcher (LocationMatcher (..))
import Arkham.Message
import Arkham.Name
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait (Trait)
import Control.Lens (non, over, set)
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
  , EntityAttrs a ~ LocationAttrs
  ) =>
  IsLocation a
  where
  toLocation :: a -> Location
  toLocation = Location

type LocationCard a = CardBuilder LocationId a

data instance Field Location :: Type -> Type where
  LocationTokens :: Field Location Tokens
  LocationClues :: Field Location Int
  LocationResources :: Field Location Int
  LocationHorror :: Field Location Int
  LocationDoom :: Field Location Int
  LocationShroud :: Field Location Int
  LocationConnectedMatchers :: Field Location [LocationMatcher]
  LocationRevealedConnectedMatchers :: Field Location [LocationMatcher]
  LocationRevealed :: Field Location Bool
  LocationConnectsTo :: Field Location (Set Direction)
  LocationCardsUnderneath :: Field Location [Card]
  LocationInvestigateSkill :: Field Location SkillType
  LocationInFrontOf :: Field Location (Maybe InvestigatorId)
  LocationCardId :: Field Location CardId
  LocationBrazier :: Field Location (Maybe Brazier)
  LocationBreaches :: Field Location (Maybe BreachStatus)
  LocationLabel :: Field Location Text
  -- virtual
  LocationTraits :: Field Location (Set Trait)
  LocationKeywords :: Field Location (Set Keyword)
  LocationUnrevealedName :: Field Location Name
  LocationName :: Field Location Name
  LocationConnectedLocations :: Field Location (Set LocationId)
  LocationCardDef :: Field Location CardDef
  LocationCard :: Field Location Card
  LocationAbilities :: Field Location [Ability]
  LocationPrintedSymbol :: Field Location LocationSymbol
  LocationVengeance :: Field Location (Maybe Int)

deriving stock instance Show (Field Location typ)
deriving stock instance Ord (Field Location typ)

fieldLens :: Field Location typ -> Lens' LocationAttrs typ
fieldLens = \case
  LocationTokens -> tokensL
  LocationClues -> tokensL . at Clue . non 0
  LocationResources -> tokensL . at Resource . non 0
  LocationHorror -> tokensL . at Horror . non 0
  LocationDoom -> tokensL . at Doom . non 0
  LocationShroud -> shroudL
  LocationConnectedMatchers -> connectedMatchersL
  LocationRevealedConnectedMatchers -> revealedConnectedMatchersL
  LocationRevealed -> revealedL
  LocationConnectsTo -> connectsToL
  LocationCardsUnderneath -> cardsUnderneathL
  LocationInvestigateSkill -> investigateSkillL
  LocationInFrontOf -> inFrontOfL
  LocationCardId -> cardIdL
  LocationBrazier -> brazierL
  LocationBreaches -> breachesL
  LocationLabel -> labelL
  LocationTraits -> virtual
  LocationKeywords -> virtual
  LocationUnrevealedName -> virtual
  LocationName -> virtual
  LocationConnectedLocations -> virtual
  LocationCardDef -> virtual
  LocationCard -> virtual
  LocationAbilities -> virtual
  LocationPrintedSymbol -> virtual
  LocationVengeance -> virtual
 where
  virtual = error "virtual attribute can not be set directly"

updateLocation :: [Update Location] -> LocationAttrs -> LocationAttrs
updateLocation updates attrs = foldr go attrs updates
 where
  go :: Update Location -> LocationAttrs -> LocationAttrs
  go (Update fld val) = set (fieldLens fld) val
  go (IncrementBy fld val) = over (fieldLens fld) (max 0 . (+ val))
  go (DecrementBy fld val) = over (fieldLens fld) (max 0 . subtract val)

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
    "LocationCardDef" -> pure $ SomeField LocationCardDef
    "LocationCard" -> pure $ SomeField LocationCard
    "LocationAbilities" -> pure $ SomeField LocationAbilities
    "LocationPrintedSymbol" -> pure $ SomeField LocationPrintedSymbol
    "LocationVengeance" -> pure $ SomeField LocationVengeance
    "LocationBrazier" -> pure $ SomeField LocationBrazier
    "LocationBreaches" -> pure $ SomeField LocationBreaches
    _ -> error "no such field"

instance Entity LocationAttrs where
  type EntityId LocationAttrs = LocationId
  type EntityAttrs LocationAttrs = LocationAttrs
  toId = locationId
  toAttrs = id
  overAttrs f = f

instance Targetable LocationAttrs where
  toTarget = LocationTarget . toId
  isTarget LocationAttrs {locationId} (LocationTarget lid) =
    locationId == lid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget attrs (BothTarget t1 t2) = isTarget attrs t1 || isTarget attrs t2
  isTarget _ _ = False

instance Is LocationAttrs LocationId where
  is = (==) . toId

instance Sourceable LocationAttrs where
  toSource = LocationSource . toId
  isSource LocationAttrs {locationId} (LocationSource lid) =
    locationId == lid
  isSource attrs (AbilitySource source _) = isSource attrs source
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
locationWith f def shroud' revealClues g =
  CardBuilder
    { cbCardCode = cdCardCode def
    , cbCardBuilder = \cardId lid ->
        (f . g)
          $ LocationAttrs
            { locationId = lid
            , locationCardCode = toCardCode def
            , locationCardId = cardId
            , locationLabel = nameToLabel (cdName def)
            , locationRevealClues = revealClues
            , locationTokens = mempty
            , locationShroud = shroud'
            , locationRevealed = not (cdDoubleSided def)
            , locationSymbol = fromJustNote "missing location symbol" (cdLocationSymbol def)
            , locationRevealedSymbol =
                fromJustNote "missing revealed location symbol" (cdLocationRevealedSymbol def)
            , locationConnectedMatchers =
                map
                  LocationWithSymbol
                  (cdLocationConnections def)
            , locationRevealedConnectedMatchers =
                map
                  LocationWithSymbol
                  (cdLocationRevealedConnections def)
            , locationDirections = mempty
            , locationConnectsTo = mempty
            , locationCardsUnderneath = mempty
            , locationCostToEnterUnrevealed = ActionCost 1
            , locationInvestigateSkill = SkillIntellect
            , locationCanBeFlipped = False
            , locationInFrontOf = Nothing
            , locationWithoutClues = False
            , locationKeys = mempty
            , locationBrazier = Nothing
            , locationBreaches = Nothing
            }
    }

locationResignAction :: LocationAttrs -> Ability
locationResignAction attrs =
  toLocationAbility
    attrs
    (mkAbility attrs 99 $ ActionAbility [Action.Resign] (ActionCost 1))

toLocationAbility :: LocationAttrs -> Ability -> Ability
toLocationAbility attrs =
  abilityCriteriaL <>~ OnLocation (LocationWithId $ toId attrs)

locationAbility :: Ability -> Ability
locationAbility ability = case abilitySource ability of
  LocationSource lid ->
    ability & abilityCriteriaL <>~ OnLocation (LocationWithId lid)
  _ -> ability

data Location = forall a. IsLocation a => Location a

instance HasField "id" Location LocationId where
  getField (Location a) = attr locationId a

instance Data Location where
  gunfold _ _ _ = error "gunfold(Location)"
  toConstr _ = error "toConstr(Location)"
  dataTypeOf _ = error "dataTypeOf(Location)"

instance Data (SomeField Location) where
  gunfold _ _ _ = error "gunfold(Location)"
  toConstr _ = error "toConstr(Location)"
  dataTypeOf _ = error "dataTypeOf(Location)"

instance Typeable a => Data (Field Location a) where
  gunfold _ _ _ = error "gunfold(Location)"
  toConstr _ = error "toConstr(Location)"
  dataTypeOf _ = error "dataTypeOf(Location)"

instance HasCardDef Location where
  toCardDef (Location l) = toCardDef (toAttrs l)

instance IsCard Location where
  toCard (Location l) = toCard (toAttrs l)
  toCardId (Location l) = toCardId (toAttrs l)
  toCardOwner (Location l) = toCardOwner (toAttrs l)

instance Eq Location where
  Location (a :: a) == Location (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Location where
  show (Location a) = show a

instance ToJSON Location where
  toJSON (Location a) = toJSON a

toLocationSymbol :: Location -> LocationSymbol
toLocationSymbol l =
  if locationRevealed attrs
    then locationRevealedSymbol attrs
    else locationSymbol attrs
 where
  attrs = toAttrs l

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

isRevealed :: Location -> Bool
isRevealed = locationRevealed . toAttrs

data SomeLocationCard where
  SomeLocationCard :: IsLocation a => LocationCard a -> SomeLocationCard

someLocationCardCode :: SomeLocationCard -> CardCode
someLocationCardCode (SomeLocationCard a) = cbCardCode a

instance Named LocationAttrs where
  toName l =
    if locationRevealed l
      then fromMaybe baseName (cdRevealedName $ toCardDef l)
      else baseName
   where
    baseName = toName (toCardDef l)

instance Named (Unrevealed LocationAttrs) where
  toName (Unrevealed l) = toName (toCardDef l)

instance IsCard LocationAttrs where
  toCard = defaultToCard
  toCardId = locationCardId
  toCardOwner = const Nothing

symbolLabel
  :: (Entity a, EntityAttrs a ~ LocationAttrs)
  => CardBuilder LocationId a
  -> CardBuilder LocationId a
symbolLabel =
  fmap
    ( overAttrs
        (\attrs -> attrs & labelL .~ (T.toLower . tshow $ locationSymbol attrs))
    )
