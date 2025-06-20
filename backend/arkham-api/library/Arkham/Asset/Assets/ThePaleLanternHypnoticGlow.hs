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
    [ mkAbility a 1 $ forced $ AssetLeavesPlay #when (be a)
    , restricted a 2 (OnSameLocation <> additionalCriteria) actionAbility
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
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust AssetLocation attrs.id
      place attrs.id $ AttachedToLocation lid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 2) iid kind (Fixed 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      takeControlOfAsset iid attrs
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thePaleLanternBeguilingAura
      pure a
    _ -> ThePaleLanternHypnoticGlow <$> liftRunMessage msg attrs
