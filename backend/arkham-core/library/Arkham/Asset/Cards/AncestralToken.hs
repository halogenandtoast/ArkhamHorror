module Arkham.Asset.Cards.AncestralToken (ancestralToken, AncestralToken (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.GameValue
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Projection

newtype AncestralToken = AncestralToken AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancestralToken :: AssetCard AncestralToken
ancestralToken = assetWith AncestralToken Cards.ancestralToken (sanityL ?~ 2)

instance HasAbilities AncestralToken where
  getAbilities (AncestralToken a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (EnemyDefeated #after You ByAny AnyEnemy) (exhaust a)
    ]

instance RunMessage AncestralToken where
  runMessage msg a@(AncestralToken attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      field EnemyHealthActual eid >>= traverse_ \healthValue -> do
        health <- getGameValue healthValue
        n <- getRemainingBlessTokens
        when (health > 0 && n > 0) do
          replicateM_ (min 5 (min n health)) $ push $ AddChaosToken #bless

      pure a
    _ -> AncestralToken <$> liftRunMessage msg attrs
