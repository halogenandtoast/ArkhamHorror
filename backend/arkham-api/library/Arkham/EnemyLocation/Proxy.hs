module Arkham.EnemyLocation.Proxy (toEnemyLocationProxy) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities (HasAbilities (..))
import Arkham.Classes.HasModifiersFor (HasModifiersFor (..))
import Arkham.Classes.RunMessage.Internal (RunMessage (..))
import Arkham.Cost (Cost (Free))
import Arkham.EnemyLocation.Cards (allEnemyLocationCards)
import Arkham.EnemyLocation.Types
import Arkham.GameValue (GameValue (Static))
import Arkham.Location.Types (
  IsLocation (..),
  Location,
  LocationAttrs (..),
 )
import Arkham.LocationSymbol (LocationSymbol (NoSymbol))
import Arkham.Prelude
import Arkham.SkillType (SkillType (SkillIntellect))

newtype EnemyLocationProxy = EnemyLocationProxy LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasCardCode EnemyLocationProxy where
  toCardCode (EnemyLocationProxy a) = locationCardCode a

instance HasCardDef EnemyLocationProxy where
  toCardDef (EnemyLocationProxy a) =
    case lookup (locationCardCode a) allEnemyLocationCards of
      Just def -> def
      Nothing -> error $ "missing card def for enemy-location proxy " <> show (locationCardCode a)

instance HasAbilities EnemyLocationProxy where
  getAbilities _ = []

instance HasModifiersFor EnemyLocationProxy where
  getModifiersFor _ = pure ()

instance RunMessage EnemyLocationProxy where
  runMessage _ p = pure p

instance IsLocation EnemyLocationProxy

toProxyLocationAttrs :: EnemyLocationAttrs -> LocationAttrs
toProxyLocationAttrs ela =
  LocationAttrs
    { locationId = enemyLocationId ela
    , locationCardCode = enemyLocationCardCode ela
    , locationCardId = enemyLocationCardId ela
    , locationLabel = enemyLocationLabel ela
    , locationRevealClues = Static 0
    , locationTokens = enemyLocationTokens ela
    , locationShroud = enemyLocationShroud ela
    , locationRevealed = True
    , locationSymbol = NoSymbol
    , locationRevealedSymbol = NoSymbol
    , locationConnectedMatchers = enemyLocationConnectedMatchers ela
    , locationRevealedConnectedMatchers = enemyLocationRevealedConnectedMatchers ela
    , locationDirections = enemyLocationDirections ela
    , locationConnectsTo = enemyLocationConnectsTo ela
    , locationCardsUnderneath = []
    , locationCostToEnterUnrevealed = Free
    , locationCanBeFlipped = False
    , locationInvestigateSkill = SkillIntellect
    , locationPlacement = enemyLocationPlacement ela
    , locationKeys = mempty
    , locationSeals = mempty
    , locationSealedChaosTokens = []
    , locationFloodLevel = Nothing
    , locationBrazier = Nothing
    , locationBreaches = Nothing
    , locationWithoutClues = True
    , locationMeta = Null
    , locationGlobalMeta = mempty
    , locationPosition = enemyLocationPosition ela
    , locationBeingRemoved = False
    , locationConcealedCards = []
    , locationOutOfGame = False
    }

toEnemyLocationProxy :: EnemyLocationAttrs -> Location
toEnemyLocationProxy = toLocation . EnemyLocationProxy . toProxyLocationAttrs
