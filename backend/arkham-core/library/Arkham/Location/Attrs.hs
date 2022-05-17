{-# LANGUAGE TemplateHaskell #-}
module Arkham.Location.Attrs where

import Arkham.Prelude

import Arkham.Ability
import Data.Aeson.TH
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards
import Arkham.LocationSymbol
import Arkham.Matcher (LocationMatcher(..))
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Data.Kind

class IsLocation a

pattern AfterFailedInvestigate :: InvestigatorId -> Target -> Message
pattern AfterFailedInvestigate iid target <-
  After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)

pattern UseResign :: InvestigatorId -> Source -> Message
pattern UseResign iid source <- UseCardAbility iid source _ 99 _

pattern UseDrawCardUnderneath :: InvestigatorId -> Source -> Message
pattern UseDrawCardUnderneath iid source <- UseCardAbility iid source _ 100 _

type LocationCard a = CardBuilder LocationId a

data instance Field LocationAttrs :: Type -> Type where
  LocationClues :: Field LocationAttrs Int

data LocationAttrs = LocationAttrs
  { locationId :: LocationId
  , locationCardCode :: CardCode
  , locationLabel :: Text
  , locationRevealClues :: GameValue Int
  , locationClues :: Int
  , locationDoom :: Int
  , locationHorror :: Int
  , locationResources :: Int
  , locationShroud :: Int
  , locationRevealed :: Bool
  , locationInvestigators :: HashSet InvestigatorId
  , locationEnemies :: HashSet EnemyId
  , locationSymbol :: LocationSymbol
  , locationRevealedSymbol :: LocationSymbol
  , locationConnectedMatchers :: [LocationMatcher]
  , locationRevealedConnectedMatchers :: [LocationMatcher]
  , locationTreacheries :: HashSet TreacheryId
  , locationEvents :: HashSet EventId
  , locationAssets :: HashSet AssetId
  , locationDirections :: HashMap Direction LocationId
  , locationConnectsTo :: HashSet Direction
  , locationCardsUnderneath :: [Card]
  , locationCostToEnterUnrevealed :: Cost
  }
  deriving stock (Show, Eq)

symbolL :: Lens' LocationAttrs LocationSymbol
symbolL = lens locationSymbol $ \m x -> m { locationSymbol = x }

costToEnterUnrevealedL :: Lens' LocationAttrs Cost
costToEnterUnrevealedL = lens locationCostToEnterUnrevealed
  $ \m x -> m { locationCostToEnterUnrevealed = x }

connectsToL :: Lens' LocationAttrs (HashSet Direction)
connectsToL = lens locationConnectsTo $ \m x -> m { locationConnectsTo = x }

connectedMatchersL :: Lens' LocationAttrs [LocationMatcher]
connectedMatchersL =
  lens locationConnectedMatchers $ \m x -> m { locationConnectedMatchers = x }

revealedConnectedMatchersL :: Lens' LocationAttrs [LocationMatcher]
revealedConnectedMatchersL = lens locationRevealedConnectedMatchers
  $ \m x -> m { locationRevealedConnectedMatchers = x }

revealedSymbolL :: Lens' LocationAttrs LocationSymbol
revealedSymbolL =
  lens locationRevealedSymbol $ \m x -> m { locationRevealedSymbol = x }

labelL :: Lens' LocationAttrs Text
labelL = lens locationLabel $ \m x -> m { locationLabel = x }

treacheriesL :: Lens' LocationAttrs (HashSet TreacheryId)
treacheriesL = lens locationTreacheries $ \m x -> m { locationTreacheries = x }

eventsL :: Lens' LocationAttrs (HashSet EventId)
eventsL = lens locationEvents $ \m x -> m { locationEvents = x }

investigatorsL :: Lens' LocationAttrs (HashSet InvestigatorId)
investigatorsL =
  lens locationInvestigators $ \m x -> m { locationInvestigators = x }

enemiesL :: Lens' LocationAttrs (HashSet EnemyId)
enemiesL = lens locationEnemies $ \m x -> m { locationEnemies = x }

assetsL :: Lens' LocationAttrs (HashSet AssetId)
assetsL = lens locationAssets $ \m x -> m { locationAssets = x }

doomL :: Lens' LocationAttrs Int
doomL = lens locationDoom $ \m x -> m { locationDoom = x }

horrorL :: Lens' LocationAttrs Int
horrorL = lens locationHorror $ \m x -> m { locationHorror = x }

cluesL :: Lens' LocationAttrs Int
cluesL = lens locationClues $ \m x -> m { locationClues = x }

resourcesL :: Lens' LocationAttrs Int
resourcesL = lens locationResources $ \m x -> m { locationResources = x }

revealedL :: Lens' LocationAttrs Bool
revealedL = lens locationRevealed $ \m x -> m { locationRevealed = x }

directionsL :: Lens' LocationAttrs (HashMap Direction LocationId)
directionsL = lens locationDirections $ \m x -> m { locationDirections = x }

cardsUnderneathL :: Lens' LocationAttrs [Card]
cardsUnderneathL =
  lens locationCardsUnderneath $ \m x -> m { locationCardsUnderneath = x }

instance Entity LocationAttrs where
  type EntityId LocationAttrs = LocationId
  type EntityAttrs LocationAttrs = LocationAttrs
  toId = locationId
  toAttrs = id

instance TargetEntity LocationAttrs where
  toTarget = LocationTarget . toId
  isTarget LocationAttrs { locationId } (LocationTarget lid) =
    locationId == lid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity LocationAttrs where
  toSource = LocationSource . toId
  isSource LocationAttrs { locationId } (LocationSource lid) =
    locationId == lid
  isSource LocationAttrs { locationId } (ProxySource (LocationSource lid) _) =
    locationId == lid
  isSource _ _ = False


instance HasCardCode LocationAttrs where
  toCardCode = locationCardCode

instance HasCardDef LocationAttrs where
  toCardDef a = case lookup (locationCardCode a) allLocationCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for location " <> show (locationCardCode a)

$(deriveJSON defaultOptions ''LocationAttrs)

unrevealed :: LocationAttrs -> Bool
unrevealed = not . locationRevealed

revealed :: LocationAttrs -> Bool
revealed = locationRevealed

location
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> CardBuilder LocationId a
location f def shroud' revealClues symbol' connectedSymbols' =
  locationWith f def shroud' revealClues symbol' connectedSymbols' id

locationWithRevealedSideConnections
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> LocationSymbol
  -> [LocationSymbol]
  -> CardBuilder LocationId a
locationWithRevealedSideConnections f def shroud' revealClues symbol' connectedSymbols' revealedSymbol' revealedConnectedSymbols'
  = locationWithRevealedSideConnectionsWith
    f
    def
    shroud'
    revealClues
    symbol'
    connectedSymbols'
    revealedSymbol'
    revealedConnectedSymbols'
    id

locationWithRevealedSideConnectionsWith
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> LocationSymbol
  -> [LocationSymbol]
  -> (LocationAttrs -> LocationAttrs)
  -> CardBuilder LocationId a
locationWithRevealedSideConnectionsWith f def shroud' revealClues symbol' connectedSymbols' revealedSymbol' revealedConnectedSymbols' g
  = locationWith
    f
    def
    shroud'
    revealClues
    symbol'
    connectedSymbols'
    (g
    . (revealedConnectedMatchersL
      <>~ map LocationWithSymbol revealedConnectedSymbols'
      )
    . (revealedSymbolL .~ revealedSymbol')
    )

locationWith
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> (LocationAttrs -> LocationAttrs)
  -> CardBuilder LocationId a
locationWith f def shroud' revealClues symbol' connectedSymbols' g =
  CardBuilder
    { cbCardCode = cdCardCode def
    , cbCardBuilder = \lid -> f . g $ LocationAttrs
      { locationId = lid
      , locationCardCode = toCardCode def
      , locationLabel = nameToLabel (cdName def)
      , locationRevealClues = revealClues
      , locationClues = 0
      , locationHorror = 0
      , locationDoom = 0
      , locationResources = 0
      , locationShroud = shroud'
      , locationRevealed = False
      , locationInvestigators = mempty
      , locationEnemies = mempty
      , locationSymbol = symbol'
      , locationRevealedSymbol = symbol'
      , locationConnectedMatchers = map LocationWithSymbol connectedSymbols'
      , locationRevealedConnectedMatchers = map
        LocationWithSymbol
        connectedSymbols'
      , locationTreacheries = mempty
      , locationEvents = mempty
      , locationAssets = mempty
      , locationDirections = mempty
      , locationConnectsTo = mempty
      , locationCardsUnderneath = mempty
      , locationCostToEnterUnrevealed = ActionCost 1
      }
    }

locationResignAction :: LocationAttrs -> Ability
locationResignAction attrs = toLocationAbility attrs (mkAbility attrs 99 $ ActionAbility (Just Action.Resign) (ActionCost 1))

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
