module Arkham.Asset.Cards.FirstAid3
  ( firstAid3
  , FirstAid3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target

newtype FirstAid3 = FirstAid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid3 :: AssetCard FirstAid3
firstAid3 = assetWith FirstAid3 Cards.firstAid3 (discardWhenNoUsesL .~ True)

instance HasAbilities FirstAid3 where
  getAbilities (FirstAid3 x) =
    [ restrictedAbility x 1 ControlsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
    ]

instance RunMessage FirstAid3 where
  runMessage msg a@(FirstAid3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      let
        componentLabel target token msgs = case target of
          InvestigatorTarget iid' ->
            ComponentLabel (InvestigatorComponent iid' token) msgs
          AssetTarget aid' -> ComponentLabel (AssetComponent aid' token) msgs
          _ -> error "invalid target type"
      targets <- liftA2
        (<>)
        (selectListMap InvestigatorTarget (InvestigatorAt YourLocation))
        (selectListMap AssetTarget (AssetAt YourLocation <> AllyAsset))
      push
        (chooseOne
          iid
          [ TargetLabel
              target
              [ chooseOne
                  iid
                  [ componentLabel target DamageToken [HealDamage target 1]
                  , componentLabel target HorrorToken [HealHorror target 1]
                  ]
              ]
          | target <- targets
          ]
        )
      pure a
    _ -> FirstAid3 <$> runMessage msg attrs
