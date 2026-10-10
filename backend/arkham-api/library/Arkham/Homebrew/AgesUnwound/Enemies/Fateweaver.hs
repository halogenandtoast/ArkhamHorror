module Arkham.Homebrew.AgesUnwound.Enemies.Fateweaver (fateweaver) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (unwarded)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTowardsMatching)
import Arkham.Projection

newtype Fateweaver = Fateweaver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Spawn__ - Nearest unwarded empty location. __Aloof__."

__Aloof__ is a printed keyword and comes off the card def. "Nearest" with no
anchor is nearest to the investigators, which is what 'NearestLocationToAny'
reads.
-}
fateweaver :: EnemyCard Fateweaver
fateweaver =
  enemyWith Fateweaver Cards.fateweaver
    $ spawnAtL
    ?~ SpawnAt (NearestLocationToAny $ unwarded <> EmptyLocation)

{- | "__Forced__ - At the start of the enemy phase: If Fateweaver is at an
unwarded location, place 1 doom on Fateweaver's location. Otherwise, move
Fateweaver once towards the nearest unwarded location."
-}
instance HasAbilities Fateweaver where
  getAbilities (Fateweaver a) =
    extend1 a $ mkAbility a 1 $ forced $ PhaseBegins #when #enemy

instance RunMessage Fateweaver where
  runMessage msg e@(Fateweaver attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mlid <- field EnemyLocation attrs.id
      for_ mlid \lid -> do
        here <- lid <=~> unwarded
        if here
          then placeDoom (attrs.ability 1) lid 1
          else
            moveTowardsMatching (attrs.ability 1) attrs
              $ NearestLocationToLocation lid unwarded
      pure e
    _ -> Fateweaver <$> liftRunMessage msg attrs
