module Arkham.Asset.Cards.StrangeSolutionRestorativeConcoction4 (
  strangeSolutionRestorativeConcoction4,
  StrangeSolutionRestorativeConcoction4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher

newtype StrangeSolutionRestorativeConcoction4 = StrangeSolutionRestorativeConcoction4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

strangeSolutionRestorativeConcoction4
  :: AssetCard StrangeSolutionRestorativeConcoction4
strangeSolutionRestorativeConcoction4 =
  asset
    StrangeSolutionRestorativeConcoction4
    Cards.strangeSolutionRestorativeConcoction4

instance HasAbilities StrangeSolutionRestorativeConcoction4 where
  getAbilities (StrangeSolutionRestorativeConcoction4 x) =
    [ restrictedAbility
        x
        1
        ( ControlsThis
            <> InvestigatorExists
              ( HealableInvestigator (toSource x) DamageType
                  $ InvestigatorAt YourLocation
              )
        )
        $ ActionAbility []
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
    ]

instance RunMessage StrangeSolutionRestorativeConcoction4 where
  runMessage msg a@(StrangeSolutionRestorativeConcoction4 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      targets <-
        selectListMap InvestigatorTarget
          $ HealableInvestigator (toSource attrs) DamageType
          $ colocatedWith iid
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel target [HealDamage target (toSource attrs) 2]
          | target <- targets
          ]
      pure a
    _ -> StrangeSolutionRestorativeConcoction4 <$> runMessage msg attrs
