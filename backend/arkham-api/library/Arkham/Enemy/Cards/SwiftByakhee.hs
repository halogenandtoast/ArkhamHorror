module Arkham.Enemy.Cards.SwiftByakhee (swiftByakhee, SwiftByakhee (..)) where

import Arkham.Ability
import Arkham.Distance
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Projection

newtype SwiftByakhee = SwiftByakhee EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftByakhee :: EnemyCard SwiftByakhee
swiftByakhee =
  enemyWith SwiftByakhee Cards.swiftByakhee (2, Static 3, 2) (1, 1)
    $ preyL
    .~ Prey LowestRemainingSanity

instance HasAbilities SwiftByakhee where
  getAbilities (SwiftByakhee a) = extend1 a $ mkAbility a 1 $ forced $ MovedFromHunter #when (be a)

instance RunMessage SwiftByakhee where
  runMessage msg e@(SwiftByakhee attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      for_ enemyLocation \loc -> do
        prey <- select (enemyPrey attrs)
        preyWithLocationsAndDistances <- forMaybeM prey \preyId -> runMaybeT do
          lid <- MaybeT $ selectOne $ locationWithInvestigator preyId
          distance <- lift $ fromMaybe (Distance 1000) <$> getDistance loc lid
          pure (preyId, lid, distance)
        chooseOrRunOneM iid do
          for_ preyWithLocationsAndDistances \(iid', pathId, distance) -> do
            targeting iid' do
              when (unDistance distance <= 1) $ phaseModifier attrs attrs CannotAttack
              moveUntil attrs pathId
      pure e
    _ -> SwiftByakhee <$> liftRunMessage msg attrs
