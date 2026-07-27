module Arkham.Enemy.Cards.SlithererInDarkness (slithererInDarkness) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Distance
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getDistance)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
      -- Not named `targets`: that would shadow Message.Lifted.Choose's `targets`,
      -- used for the tie-break below.
      lowestCombat <- select (atConnected attrs.id)
      if notNull lowestCombat
        then for_ lowestCombat \iid ->
          push
            $ EnemyWillAttack
            $ (enemyAttack attrs.id (attrs.ability 1) iid)
              { attackDamageStrategy = enemyDamageStrategy attrs
              }
        else do
          -- No attack was made: move directly to the flooded location nearest the
          -- Moving Platform. Several can be equally near, and the card does not say
          -- which to pick, so the lead investigator decides; chooseOrRunOneM resolves
          -- silently when only one location is tied for nearest.
          mPlatform <- selectOne $ locationIs Locations.movingPlatformObservationStation
          for_ mPlatform \platform -> do
            flooded <- select FloodedLocation
            withDist <- forMaybeM flooded \lid ->
              fmap ((,lid) . unDistance) <$> getDistance platform lid
            for_ (minimumMay $ map fst withDist) \nearest -> do
              lead <- getLead
              chooseOrRunOneM lead
                $ targets [lid | (d, lid) <- withDist, d == nearest]
                $ enemyMoveTo (attrs.ability 1) attrs.id
      pure e
    _ -> SlithererInDarkness <$> liftRunMessage msg attrs
