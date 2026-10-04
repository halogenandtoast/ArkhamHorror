module Arkham.Homebrew.AgainstTheWendigo.Enemies.WildIndigenous (wildIndigenous) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wild)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveToMatch)

newtype WildIndigenous = WildIndigenous EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wildIndigenous :: EnemyCard WildIndigenous
wildIndigenous = enemy WildIndigenous Cards.wildIndigenous & setPrey LowestRemainingSanity

instance HasAbilities WildIndigenous where
  getAbilities (WildIndigenous a) =
    [restricted a 1 (exists $ be a <> EnemyIsEngagedWith Anyone) $ forced $ PhaseEnds #when #enemy]

instance RunMessage WildIndigenous where
  runMessage msg e@(WildIndigenous attrs) = runQueueT $ case msg of
    -- "Disengage them and move them to the nearest Wild location."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      disengageFromAll attrs
      selectForMaybeM (locationWithEnemy attrs) \lid ->
        enemyMoveToMatch attrs attrs (NearestLocationToLocation lid (LocationWithTrait Wild))
      pure e
    _ -> WildIndigenous <$> liftRunMessage msg attrs
