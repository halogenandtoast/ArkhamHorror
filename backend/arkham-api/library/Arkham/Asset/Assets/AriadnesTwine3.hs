module Arkham.Asset.Assets.AriadnesTwine3 (ariadnesTwine3, AriadnesTwine3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype AriadnesTwine3 = AriadnesTwine3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ariadnesTwine3 :: AssetCard AriadnesTwine3
ariadnesTwine3 = asset AriadnesTwine3 Cards.ariadnesTwine3

instance HasModifiersFor AriadnesTwine3 where
  getModifiersFor (AriadnesTwine3 a) =
    case a.controller of
      Nothing -> pure mempty
      Just iid -> do
        modifySelectWhen
          a
          (a.use Secret > 0)
          (not_ (be a) <> AssetControlledBy (affectsOthers $ colocatedWith iid))
          [ProvidesUses Secret (toSource a)]

instance HasAbilities AriadnesTwine3 where
  getAbilities (AriadnesTwine3 a) =
    [ controlledAbility
        a
        1
        ( oneOf
            [ exists (AssetControlledBy You <> oneOf [AssetWithUseType Secret, AssetWithoutUses])
                <> youExist (InvestigatorWithResources $ atLeast 1)
            ]
        )
        $ FastAbility (exhaust a)
    ]

instance RunMessage AriadnesTwine3 where
  runMessage msg a@(AriadnesTwine3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasResources <- iid <=~> InvestigatorWithResources (atLeast 1)
      canGainResources <- can.gain.resources iid
      assets <- select $ assetControlledBy iid <> AssetWithUses Secret
      chooseOrRunOne
        iid
        $ [ Label "Move 1 secret from an asset you control to your resource pool, as a resource" [DoStep 1 msg]
          | notNull assets
          , canGainResources
          ]
        <> [ Label "Move 1 resource from your resource pool to an asset you control, as a secret" [DoStep 2 msg]
           | hasResources
           ]

      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      assets <- select $ assetControlledBy iid <> AssetWithUses Secret
      chooseOrRunOne
        iid
        [ targetLabel target [MoveTokens (attrs.ability 1) (toSource target) (ResourceTarget iid) Secret 1]
        | target <- assets
        ]
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      assets <- select $ assetControlledBy iid <> oneOf [AssetWithUseType Secret, AssetWithoutUses]
      chooseOrRunOne
        iid
        [ targetLabel target [MoveTokens (attrs.ability 1) (ResourceSource iid) (toTarget target) Secret 1]
        | target <- assets
        ]
      pure a
    _ -> AriadnesTwine3 <$> liftRunMessage msg attrs
