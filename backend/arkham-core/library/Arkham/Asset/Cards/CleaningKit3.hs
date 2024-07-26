module Arkham.Asset.Cards.CleaningKit3 (cleaningKit3, CleaningKit3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Matcher

newtype CleaningKit3 = CleaningKit3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleaningKit3 :: AssetCard CleaningKit3
cleaningKit3 = asset CleaningKit3 Cards.cleaningKit3

instance HasModifiersFor CleaningKit3 where
  getModifiersFor (AssetTarget aid) (CleaningKit3 a) | a.id /= aid = maybeModified a do
    iid <- hoistMaybe a.controller
    liftGuardM $ aid <=~> assetControlledBy iid
    guard $ a.use Supply > 0
    pure [ProvidesUses Supply (toSource a), ProvidesProxyUses Supply Ammo (toSource a)]
  getModifiersFor _ _ = pure []

instance HasAbilities CleaningKit3 where
  getAbilities (CleaningKit3 x) =
    [ controlledAbility x 1 (exists $ ActiveAbility <> AbilityIsSkillTest)
        $ ReactionAbility (SpentUses #when Anyone AnySource Supply (be x) (atLeast 1)) (exhaust x)
    ]

instance RunMessage CleaningKit3 where
  runMessage msg a@(CleaningKit3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nextSkillTestModifier (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    _ -> CleaningKit3 <$> liftRunMessage msg attrs
