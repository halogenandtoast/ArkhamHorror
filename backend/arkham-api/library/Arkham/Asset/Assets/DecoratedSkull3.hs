module Arkham.Asset.Assets.DecoratedSkull3 (decoratedSkull3, DecoratedSkull3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (
  AssetDefeated,
  EnemyDefeated,
  InvestigatorDefeated,
 )
import Arkham.Matcher
import Arkham.Prelude

newtype DecoratedSkull3 = DecoratedSkull3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoratedSkull3 :: AssetCard DecoratedSkull3
decoratedSkull3 = asset DecoratedSkull3 Cards.decoratedSkull3

instance HasAbilities DecoratedSkull3 where
  getAbilities (DecoratedSkull3 a) =
    [ restricted a 1 ControlsThis
        $ freeReaction
        $ oneOf
          [ EnemyDefeated #after Anyone ByAny (EnemyAt YourLocation)
          , InvestigatorDefeated #after ByAny (InvestigatorAt YourLocation)
          , AssetDefeated #after ByAny (AllyAsset <> AssetAt YourLocation)
          ]
    , restricted a 2 ControlsThis
        $ actionAbilityWithCost (UpTo (Fixed 3) $ assetUseCost a Charge 1)
    ]

instance RunMessage DecoratedSkull3 where
  runMessage msg a@(DecoratedSkull3 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AddUses (attrs.ability 1) (toId attrs) Charge 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (totalUsesPayment -> n) -> do
      let drawing = drawCards iid (attrs.ability 2) n
      pushAll [drawing, TakeResources iid n (attrs.ability 2) False]
      pure a
    _ -> DecoratedSkull3 <$> runMessage msg attrs
