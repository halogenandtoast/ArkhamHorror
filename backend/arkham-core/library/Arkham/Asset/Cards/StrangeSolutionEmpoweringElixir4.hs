module Arkham.Asset.Cards.StrangeSolutionEmpoweringElixir4 (
  strangeSolutionEmpoweringElixir4,
  StrangeSolutionEmpoweringElixir4 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype StrangeSolutionEmpoweringElixir4 = StrangeSolutionEmpoweringElixir4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionEmpoweringElixir4 :: AssetCard StrangeSolutionEmpoweringElixir4
strangeSolutionEmpoweringElixir4 =
  asset StrangeSolutionEmpoweringElixir4 Cards.strangeSolutionEmpoweringElixir4

instance HasAbilities StrangeSolutionEmpoweringElixir4 where
  getAbilities (StrangeSolutionEmpoweringElixir4 attrs) =
    [ controlledAbility
        attrs
        1
        ( InvestigatorExists
            $ InvestigatorAt YourLocation
              <> AnyInvestigator [InvestigatorCanGainResources, InvestigatorCanDrawCards Anyone]
        )
        actionAbility
    ]

instance RunMessage StrangeSolutionEmpoweringElixir4 where
  runMessage msg a@(StrangeSolutionEmpoweringElixir4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      investigators <- selectList $ colocatedWith iid
      choices <- forMaybeM investigators $ \i -> do
        mGainResources <- gainResourcesIfCan i source 2
        mDrawCards <- drawCardsIfCan i source 1
        pure $ (i,) <$> ((:) <$> mGainResources <*> ((:) <$> mDrawCards <*> pure []))

      if null choices
        then error "invalid call"
        else push $ chooseOrRunOne iid $ map (uncurry targetLabel) choices
      pure a
    _ -> StrangeSolutionEmpoweringElixir4 <$> runMessage msg attrs
