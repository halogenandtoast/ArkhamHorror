module Arkham.Asset.Cards.ForbiddenKnowledge where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype ForbiddenKnowledge = ForbiddenKnowledge AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenKnowledge :: AssetCard ForbiddenKnowledge
forbiddenKnowledge =
  assetWith
    ForbiddenKnowledge
    Cards.forbiddenKnowledge
    (discardWhenNoUsesL .~ True)

instance HasAbilities ForbiddenKnowledge where
  getAbilities (ForbiddenKnowledge a) =
    [ restrictedAbility
        (toSource a)
        1
        ControlsThis
        ( FastAbility $
            Costs
              [ UseCost (AssetWithId $ toId a) Secret 1
              , HorrorCost (toSource a) YouTarget 1
              , ExhaustCost (toTarget a)
              ]
        )
    ]

instance RunMessage ForbiddenKnowledge where
  runMessage msg a@(ForbiddenKnowledge attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 1 (toAbilitySource attrs 1) False
      pure a
    _ -> ForbiddenKnowledge <$> runMessage msg attrs
