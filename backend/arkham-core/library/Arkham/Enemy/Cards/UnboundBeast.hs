module Arkham.Enemy.Cards.UnboundBeast (
  unboundBeast,
  UnboundBeast (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection

newtype UnboundBeast = UnboundBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

unboundBeast :: EnemyCard UnboundBeast
unboundBeast =
  enemyWith
    UnboundBeast
    Cards.unboundBeast
    (3, Static 3, 3)
    (1, 1)
    (\a -> a & preyL .~ BearerOf (toId a))

instance RunMessage UnboundBeast where
  runMessage msg e@(UnboundBeast attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      summonedHounds <-
        selectList (assetIs Assets.summonedHound1) >>= traverse \hound -> do
          controller <- field AssetController hound
          card <- field AssetCard hound
          pure (hound, card, controller)
      if null summonedHounds
        then push $ PlaceInBonded iid (toCard attrs)
        else do
          player <- getPlayer iid
          push
            $ chooseOrRunOne
              player
              [ targetLabel hound [PlaceInBonded controller card, EnemyEngageInvestigator (toId attrs) controller]
              | (hound, card, mController) <- summonedHounds
              , controller <- toList mController
              ]
      pure e
    _ -> UnboundBeast <$> runMessage msg attrs
