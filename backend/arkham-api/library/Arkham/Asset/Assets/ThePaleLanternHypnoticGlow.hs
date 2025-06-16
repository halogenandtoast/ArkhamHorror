module Arkham.Asset.Assets.ThePaleLanternHypnoticGlow (
  thePaleLanternHypnoticGlow,
  ThePaleLanternHypnoticGlow(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as EnemyCards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ThePaleLanternHypnoticGlow = ThePaleLanternHypnoticGlow AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Hypnotic Glow side of The Pale Lantern (#71068).
thePaleLanternHypnoticGlow :: AssetCard ThePaleLanternHypnoticGlow
thePaleLanternHypnoticGlow =
  storyAsset ThePaleLanternHypnoticGlow Cards.thePaleLanternHypnoticGlow

instance HasAbilities ThePaleLanternHypnoticGlow where
  getAbilities (ThePaleLanternHypnoticGlow a) =
    [ mkAbility a 1 $ forced $ AssetLeavesPlay #when (be a)
    , restrictedAbility a 2 (Here <> additionalCriteria) $ actionAbility
    ]
   where
    additionalCriteria =
      exists (enemyIs EnemyCards.theBloodlessMan <> EnemyWithAnyDamage)
        <> exists (VictoryDisplayCardMatch $ basic $ cardIs EnemyCards.theBloodlessMan)

instance RunMessage ThePaleLanternHypnoticGlow where
  runMessage msg a@(ThePaleLanternHypnoticGlow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust AssetLocation attrs.id
      place attrs.id $ AttachedToLocation lid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOne iid
        [ Label "Use your {combat}" $ beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 1)
        , Label "Use your {agility}" $ beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 1)
        ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ TakeControlOfAsset iid attrs.id
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    _ -> ThePaleLanternHypnoticGlow <$> liftRunMessage msg attrs
