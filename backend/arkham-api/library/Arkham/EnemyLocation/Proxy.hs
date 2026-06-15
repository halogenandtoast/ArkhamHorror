module Arkham.EnemyLocation.Proxy (toEnemyLocationProxy) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities (HasAbilities (..))
import Arkham.Classes.HasModifiersFor (HasModifiersFor (..))
import Arkham.Classes.RunMessage.Internal (RunMessage (..))
import Arkham.EnemyLocation.Cards (allEnemyLocationCards)
import Arkham.EnemyLocation.Types
import Arkham.Location.Base (LocationAttrs (..))
import Arkham.Location.Types (IsLocation (..), Location, toLocation)
import Arkham.Matcher.Location (LocationMatcher (LocationWithId))
import Arkham.Prelude

newtype EnemyLocationProxy = EnemyLocationProxy LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasCardCode EnemyLocationProxy where
  toCardCode (EnemyLocationProxy a) = a.cardCode

instance HasCardDef EnemyLocationProxy where
  toCardDef (EnemyLocationProxy a) =
    case lookup a.cardCode allEnemyLocationCards of
      Just def -> def
      Nothing -> error $ "missing card def for enemy-location proxy " <> show a.cardCode

instance HasAbilities EnemyLocationProxy where
  getAbilities _ = []

instance HasModifiersFor EnemyLocationProxy where
  getModifiersFor _ = pure ()

instance RunMessage EnemyLocationProxy where
  runMessage _ p = pure p

instance IsLocation EnemyLocationProxy

-- | Project an enemy-location into a regular 'Location' for the engine's
-- location queries. Enemy-locations store their grid adjacency in
-- 'locationDirections' (e.g. Shapeless Cellar connects up to the Foyer) but
-- leave 'locationConnectsTo'/'locationConnectedMatchers' empty, so the engine's
-- connectivity (movement, 'NearestLocationTo', etc.) would treat them as
-- isolated. Mirror 'withEnemyLocationAsLocationData' by surfacing the
-- directional connections as connected matchers on the proxy.
toEnemyLocationProxy :: EnemyLocationAttrs -> Location
toEnemyLocationProxy attrs = toLocation $ EnemyLocationProxy base'
 where
  base = enemyLocationBase attrs
  directionConnectedIds = concatMap snd $ mapToList (locationDirections base)
  extraMatchers = map LocationWithId directionConnectedIds
  base' =
    base
      { locationConnectedMatchers = locationConnectedMatchers base <> extraMatchers
      , locationRevealedConnectedMatchers = locationRevealedConnectedMatchers base <> extraMatchers
      }
