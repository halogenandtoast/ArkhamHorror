module Arkham.Asset.Assets.DoNoHarmReliableSupport (doNoHarmCompleted) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype DoNoHarmReliableSupport = DoNoHarmReliableSupport AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doNoHarmCompleted :: AssetCard DoNoHarmReliableSupport
doNoHarmCompleted = asset DoNoHarmReliableSupport Cards.doNoHarmCompleted

instance HasModifiersFor DoNoHarmReliableSupport where
  getModifiersFor (DoNoHarmReliableSupport a) =
    for_ a.controller \iid -> modified_ a iid [SkillModifier #agility 1]

-- After you heal damage/horror from an investigator or Ally asset.
healed :: WindowMatcher
healed =
  oneOf
    $ [InvestigatorHealed #after dt Anyone (SourceOwnedBy You) | dt <- [#damage, #horror]]
    <> [AssetHealed #after dt #ally (SourceOwnedBy You) | dt <- [#damage, #horror]]

instance HasAbilities DoNoHarmReliableSupport where
  getAbilities (DoNoHarmReliableSupport a) =
    [controlled a 1 NoRestriction $ triggered healed (exhaust a)]

instance RunMessage DoNoHarmReliableSupport where
  runMessage msg a@(DoNoHarmReliableSupport attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> DoNoHarmReliableSupport <$> liftRunMessage msg attrs
