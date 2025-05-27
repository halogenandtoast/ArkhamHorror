module Arkham.Asset.Assets.YhanthleiStatueDynamicRelic (yhanthleiStatueDynamicRelic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Humanoid))

newtype YhanthleiStatueDynamicRelic = YhanthleiStatueDynamicRelic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yhanthleiStatueDynamicRelic :: AssetCard YhanthleiStatueDynamicRelic
yhanthleiStatueDynamicRelic = asset YhanthleiStatueDynamicRelic Cards.yhanthleiStatueDynamicRelic

instance HasAbilities YhanthleiStatueDynamicRelic where
  getAbilities (YhanthleiStatueDynamicRelic x) =
    [ controlled
        x
        1
        (exists $ withTrait Humanoid <> at_ YourLocation <> EnemyCanBeDamagedBySource (x.ability 1))
        $ FastAbility (exhaust x)
    , controlledAbility x 2 (HasRemainingBlessTokens <> ChaosTokenCountIs #curse (atLeast 1))
        $ FastAbility (exhaust x)
    ]

instance RunMessage YhanthleiStatueDynamicRelic where
  runMessage msg a@(YhanthleiStatueDynamicRelic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ withTrait Humanoid
          <> enemyAtLocationWith iid
          <> EnemyCanBeDamagedBySource (attrs.ability 1)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      n <- (min 2 .) . min <$> getRemainingBlessTokens <*> selectCount (ChaosTokenFaceIs #curse)
      repeated n $ removeChaosToken #curse
      repeated n $ addChaosToken #bless
      pure a
    _ -> YhanthleiStatueDynamicRelic <$> liftRunMessage msg attrs
