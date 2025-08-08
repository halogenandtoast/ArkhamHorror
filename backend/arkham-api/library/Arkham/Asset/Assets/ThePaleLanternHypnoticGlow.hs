module Arkham.Asset.Assets.ThePaleLanternHypnoticGlow (thePaleLanternHypnoticGlow) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection

newtype ThePaleLanternHypnoticGlow = ThePaleLanternHypnoticGlow AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Hypnotic Glow side of The Pale Lantern (#71068).
thePaleLanternHypnoticGlow :: AssetCard ThePaleLanternHypnoticGlow
thePaleLanternHypnoticGlow = asset ThePaleLanternHypnoticGlow Cards.thePaleLanternHypnoticGlow

instance HasAbilities ThePaleLanternHypnoticGlow where
  getAbilities (ThePaleLanternHypnoticGlow a) =
    [ restricted a 1 (OnSameLocation <> additionalCriteria) actionAbility
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]
   where
    additionalCriteria =
      oneOf
        [ exists
            ( mapOneOf enemyIs [Enemies.theBloodlessMan, Enemies.theBloodlessManUnleashed]
                <> EnemyWithAnyDamage
            )
        , exists
            ( VictoryDisplayCardMatch
                $ basic
                $ mapOneOf cardIs [Enemies.theBloodlessMan, Enemies.theBloodlessManUnleashed]
            )
        ]

instance RunMessage ThePaleLanternHypnoticGlow where
  runMessage msg a@(ThePaleLanternHypnoticGlow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#combat, #agility] (Fixed 3)
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 ws _ -> do
      don'tRemove attrs ws
      lid <- fieldJust AssetLocation attrs.id
      place attrs.id $ AttachedToLocation lid
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfAsset iid attrs
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thePaleLanternBeguilingAura
      pure a
    _ -> ThePaleLanternHypnoticGlow <$> liftRunMessage msg attrs
