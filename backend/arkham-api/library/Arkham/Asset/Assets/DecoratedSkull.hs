module Arkham.Asset.Assets.DecoratedSkull (decoratedSkull) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated, EnemyDefeated, InvestigatorDefeated)
import Arkham.Asset.Uses
import Arkham.Matcher

newtype DecoratedSkull = DecoratedSkull AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoratedSkull :: AssetCard DecoratedSkull
decoratedSkull = asset DecoratedSkull Cards.decoratedSkull

instance HasAbilities DecoratedSkull where
  getAbilities (DecoratedSkull a) =
    [ controlled_ a 1
        $ freeReaction
        $ oneOf
          [ EnemyDefeated #after Anyone ByAny (EnemyAt YourLocation)
          , InvestigatorDefeated #after ByAny (colocatedWithMatch You)
          , AssetDefeated #after ByAny (AllyAsset <> AssetAt YourLocation)
          ]
    , controlled_ a 2 (actionAbilityWithCost $ assetUseCost a Charge 1)
    ]

instance RunMessage DecoratedSkull where
  runMessage msg a@(DecoratedSkull attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addUses (attrs.ability 1) attrs Charge 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawCards iid (attrs.ability 2) 1
      gainResources iid (attrs.ability 2) 1
      pure a
    _ -> DecoratedSkull <$> liftRunMessage msg attrs
