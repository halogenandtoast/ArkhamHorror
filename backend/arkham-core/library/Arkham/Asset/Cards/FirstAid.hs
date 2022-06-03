module Arkham.Asset.Cards.FirstAid
  ( FirstAid(..)
  , firstAid
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target

newtype FirstAid = FirstAid AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetCard FirstAid
firstAid = assetWith FirstAid Cards.firstAid (discardWhenNoUsesL .~ True)

instance HasAbilities FirstAid where
  getAbilities (FirstAid x) =
    [ restrictedAbility x 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
    ]

instance AssetRunner env => RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- selectListMap InvestigatorTarget $ InvestigatorAt YourLocation
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              target
              [chooseOne iid [HealDamage target 1, HealHorror target 1]]
          | target <- targets
          ]
        )
    _ -> FirstAid <$> runMessage msg attrs
