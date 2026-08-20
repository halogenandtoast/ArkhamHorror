module Arkham.Enemy.Cards.TheScarletKeys.OnThinIce.RavenousGrizzly (ravenousGrizzly) where

import Arkham.Enemy.CardDefs.TheScarletKeys.OnThinIce qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf, modifySelfMaybe)
import Arkham.Location.CardDefs.TheScarletKeys.OnThinIce qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Wilderness))

newtype RavenousGrizzly = RavenousGrizzly EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ravenousGrizzly :: EnemyCard RavenousGrizzly
ravenousGrizzly =
  enemy RavenousGrizzly Cards.ravenousGrizzly
    & setSpawnAt (NearestLocationToYou $ LocationWithTrait Wilderness)

instance HasModifiersFor RavenousGrizzly where
  getModifiersFor (RavenousGrizzly a) = do
    nonWilderness <- select $ not_ $ LocationWithTrait Wilderness
    modifySelf a $ map CannotEnter nonWilderness
    alaskanWilderness <-
      select
        $ mapOneOf locationIs [Locations.mountainStream, Locations.frozenLake, Locations.isolatedRoad]
    modifySelfMaybe a do
      loc <- MaybeT $ getLocationOf a
      guard $ loc `elem` alaskanWilderness
      pure [HunterConnectedTo x | x <- alaskanWilderness, x /= loc]
