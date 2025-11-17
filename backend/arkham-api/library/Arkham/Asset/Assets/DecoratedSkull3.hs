module Arkham.Asset.Assets.DecoratedSkull3 (decoratedSkull3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated, EnemyDefeated, InvestigatorDefeated)
import Arkham.Asset.Uses
import Arkham.Matcher

newtype DecoratedSkull3 = DecoratedSkull3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoratedSkull3 :: AssetCard DecoratedSkull3
decoratedSkull3 = asset DecoratedSkull3 Cards.decoratedSkull3

instance HasAbilities DecoratedSkull3 where
  getAbilities (DecoratedSkull3 a) =
    [ controlled_ a 1
        $ freeReaction
        $ oneOf
          [ EnemyDefeated #after Anyone ByAny (EnemyAt YourLocation)
          , InvestigatorDefeated #after ByAny (colocatedWithMatch You)
          , AssetDefeated #after ByAny (AllyAsset <> AssetAt YourLocation)
          ]
    , controlled_ a 2 $ actionAbilityWithCost (UpTo (Fixed 3) $ assetUseCost a Charge 1)
    ]

instance RunMessage DecoratedSkull3 where
  runMessage msg a@(DecoratedSkull3 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addUses (attrs.ability 1) (toId attrs) Charge 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (totalUsesPayment -> n) -> do
      drawCards iid (attrs.ability 2) n
      gainResources iid (attrs.ability 2) n
      pure a
    _ -> DecoratedSkull3 <$> liftRunMessage msg attrs
