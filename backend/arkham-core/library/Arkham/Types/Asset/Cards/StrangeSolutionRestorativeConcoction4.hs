module Arkham.Types.Asset.Cards.StrangeSolutionRestorativeConcoction4
  ( strangeSolutionRestorativeConcoction4
  , StrangeSolutionRestorativeConcoction4(..)
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
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype StrangeSolutionRestorativeConcoction4 = StrangeSolutionRestorativeConcoction4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionRestorativeConcoction4
  :: AssetCard StrangeSolutionRestorativeConcoction4
strangeSolutionRestorativeConcoction4 = asset
  StrangeSolutionRestorativeConcoction4
  Cards.strangeSolutionRestorativeConcoction4

instance HasAbilities StrangeSolutionRestorativeConcoction4 where
  getAbilities (StrangeSolutionRestorativeConcoction4 x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> InvestigatorExists
          (InvestigatorAt YourLocation <> InvestigatorWithAnyDamage)
        )
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, UseCost (toId x) Supply 1]
    ]

instance AssetRunner env => RunMessage env StrangeSolutionRestorativeConcoction4 where
  runMessage msg a@(StrangeSolutionRestorativeConcoction4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      targets <- map InvestigatorTarget
        <$> getSetList @InvestigatorId locationId
      a <$ push
        (chooseOne
          iid
          [ TargetLabel target [HealDamage target 2] | target <- targets ]
        )
    _ -> StrangeSolutionRestorativeConcoction4 <$> runMessage msg attrs
