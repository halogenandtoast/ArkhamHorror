module Arkham.Homebrew.CircusExMortis.Locations.ShadowedWilderness_173 (
  shadowedWilderness_173,
) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (nearestEnemiesAbleToMoveToward)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype ShadowedWilderness_173 = ShadowedWilderness_173 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowedWilderness_173 :: LocationCard ShadowedWilderness_173
shadowedWilderness_173 = location ShadowedWilderness_173 Cards.shadowedWilderness_173 1 (PerPlayer 1)

instance HasAbilities ShadowedWilderness_173 where
  getAbilities (ShadowedWilderness_173 a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage ShadowedWilderness_173 where
  runMessage msg l@(ShadowedWilderness_173 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "the nearest" enemy is singular, so the investigator picks among ties.
      movers <- nearestEnemiesAbleToMoveToward attrs.id NonEliteEnemy
      if null movers
        then drawEncounterCard iid (attrs.ability 1)
        else chooseTargetM iid movers \enemy -> moveToward enemy (LocationWithId attrs.id)
      pure l
    _ -> ShadowedWilderness_173 <$> liftRunMessage msg attrs
