module Arkham.Enemy.Cards.UnboundBeast (unboundBeast) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype UnboundBeast = UnboundBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

unboundBeast :: EnemyCard UnboundBeast
unboundBeast =
  enemyWith UnboundBeast Cards.unboundBeast (3, Static 3, 3) (1, 1)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance RunMessage UnboundBeast where
  runMessage msg e@(UnboundBeast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      summonedHounds <-
        select (assetIs Assets.summonedHound1) >>= traverse \hound -> do
          controller <- field AssetController hound
          card <- field AssetCard hound
          pure (hound, card, controller)
      if null summonedHounds
        then push $ PlaceInBonded iid (toCard attrs)
        else do
          chooseOrRunOneM iid do
            for_ summonedHounds \(hound, card, mController) -> do
              for_ mController \controller -> do
                targeting hound do
                  pushAll [PlaceInBonded controller card, EnemyEngageInvestigator (toId attrs) controller]
      pure e
    _ -> UnboundBeast <$> liftRunMessage msg attrs
