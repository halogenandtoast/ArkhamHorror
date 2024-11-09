module Arkham.Asset.Assets.YhanthleiStatueMysteriousRelic (
  yhanthleiStatueMysteriousRelic,
  YhanthleiStatueMysteriousRelic (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Humanoid))

newtype YhanthleiStatueMysteriousRelic = YhanthleiStatueMysteriousRelic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yhanthleiStatueMysteriousRelic :: AssetCard YhanthleiStatueMysteriousRelic
yhanthleiStatueMysteriousRelic =
  assetWith YhanthleiStatueMysteriousRelic Cards.yhanthleiStatueMysteriousRelic
    $ (isStoryL .~ True)

instance HasAbilities YhanthleiStatueMysteriousRelic where
  getAbilities (YhanthleiStatueMysteriousRelic x) =
    [ controlledAbility
        x
        1
        (exists $ EnemyWithTrait Humanoid <> at_ YourLocation <> EnemyCanBeDamagedBySource (x.ability 1))
        $ FastAbility (exhaust x <> AddCurseTokenCost 2)
    , restricted x 2 ControlsThis
        $ actionAbilityWithCost (OrCost $ map SpendKeyCost [BlueKey, WhiteKey, RedKey, YellowKey])
    ]

instance RunMessage YhanthleiStatueMysteriousRelic where
  runMessage msg a@(YhanthleiStatueMysteriousRelic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ withTrait Humanoid
          <> enemyAtLocationWith iid
          <> EnemyCanBeDamagedBySource (attrs.ability 1)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.yhanthleiStatueDynamicRelic
      pure a
    _ -> YhanthleiStatueMysteriousRelic <$> liftRunMessage msg attrs
