module Arkham.Asset.Cards.FirstAid3
  ( firstAid3
  , FirstAid3(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target

newtype FirstAid3 = FirstAid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid3 :: AssetCard FirstAid3
firstAid3 =
  assetWith FirstAid3 Cards.firstAid3 (discardWhenNoUsesL .~ True)

instance HasAbilities FirstAid3 where
  getAbilities (FirstAid3 x) =
    [ restrictedAbility x 1 ControlsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
    ]

instance RunMessage FirstAid3 where
  runMessage msg a@(FirstAid3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      targets <- liftA2
        (<>)
        (selectListMap InvestigatorTarget (InvestigatorAt YourLocation))
        (selectListMap AssetTarget (AssetAt YourLocation <> AllyAsset))
      push
        (chooseOne
          iid
          [ TargetLabel
              target
              [chooseOne iid [HealDamage target 1, HealHorror target 1]]
          | target <- targets
          ]
        )
      pure a
    _ -> FirstAid3 <$> runMessage msg attrs
