module Arkham.Types.Asset.Cards.FirstAid
  ( FirstAid(..)
  , firstAid
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype FirstAid = FirstAid AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetCard FirstAid
firstAid = asset FirstAid Cards.firstAid

instance HasAbilities FirstAid where
  getAbilities (FirstAid x) =
    [ restrictedAbility x 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, UseCost (toId x) Supply 1]
    ]

instance AssetRunner env => RunMessage env FirstAid where
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
