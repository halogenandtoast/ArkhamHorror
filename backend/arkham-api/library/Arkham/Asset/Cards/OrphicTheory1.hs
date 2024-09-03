module Arkham.Asset.Cards.OrphicTheory1 (orphicTheory1, OrphicTheory1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype OrphicTheory1 = OrphicTheory1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orphicTheory1 :: AssetCard OrphicTheory1
orphicTheory1 = asset OrphicTheory1 Cards.orphicTheory1

validTreacheries :: TreacheryMatcher
validTreacheries = TreacheryIsNonWeakness <> not_ (TreacheryOnEnemy EliteEnemy) <> not_ HiddenTreachery

instance HasAbilities OrphicTheory1 where
  getAbilities (OrphicTheory1 a) =
    [controlledAbility a 1 (exists validTreacheries) $ FastAbility (assetUseCost a Secret 1)]

instance RunMessage OrphicTheory1 where
  runMessage msg a@(OrphicTheory1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1) validTreacheries
      pure a
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (TreacheryTarget tid) -> do
      roundModifier (attrs.ability 1) tid Blank
      pure a
    _ -> OrphicTheory1 <$> liftRunMessage msg attrs
