module Arkham.Enemy.Cards.SlithererInDarkness (slithererInDarkness) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Distance
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getDistance)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveTo)

newtype SlithererInDarkness = SlithererInDarkness EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slithererInDarkness :: EnemyCard SlithererInDarkness
slithererInDarkness =
  enemy SlithererInDarkness Cards.slithererInDarkness
    & setSpawnAt (NearestLocationToYou FloodedLocation)

instance HasModifiersFor SlithererInDarkness where
  getModifiersFor (SlithererInDarkness a) = do
    -- "cannot enter unflooded locations"
    unflooded <- select $ not_ FloodedLocation
    modifySelf a $ map CannotEnter unflooded

atConnected :: EnemyId -> InvestigatorMatcher
atConnected eid =
  InvestigatorWithLowestSkill #combat
    $ InvestigatorAt (connectedFrom $ locationWithEnemy eid)

instance HasAbilities SlithererInDarkness where
  getAbilities (SlithererInDarkness a) =
    extend1 a
      $ restricted a 1 (notExists $ InvestigatorAt (locationWithEnemy a.id))
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage SlithererInDarkness where
  runMessage msg e@(SlithererInDarkness attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      targets <- select (atConnected attrs.id)
      if notNull targets
        then for_ targets \iid ->
          push
            $ EnemyWillAttack
            $ (enemyAttack attrs.id (attrs.ability 1) iid)
              {attackDamageStrategy = enemyDamageStrategy attrs}
        else do
          -- No attack was made: move directly to the flooded location nearest the
          -- Moving Platform.
          mPlatform <- selectOne $ locationIs Locations.movingPlatformObservationStation
          for_ mPlatform \platform -> do
            flooded <- select FloodedLocation
            withDist <- forMaybeM flooded \lid ->
              fmap ((,lid) . unDistance) <$> getDistance platform lid
            case sortOn fst withDist of
              [] -> pure ()
              (_, nearest) : _ -> enemyMoveTo (attrs.ability 1) attrs.id nearest
      pure e
    _ -> SlithererInDarkness <$> liftRunMessage msg attrs
