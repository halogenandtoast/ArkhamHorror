module Arkham.Asset.Cards.StrangeSolutionEmpoweringElixir4 (
  strangeSolutionEmpoweringElixir4,
  StrangeSolutionEmpoweringElixir4 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
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
        ( exists
            $ affectsOthers
            $ InvestigatorAt YourLocation
            <> oneOf [can.gain.resources, can.draw.cards]
        )
        $ actionAbilityWithCost (assetUseCost attrs Charge 1 <> exhaust attrs)
    ]

instance RunMessage StrangeSolutionEmpoweringElixir4 where
  runMessage msg a@(StrangeSolutionEmpoweringElixir4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      investigators <- select $ affectsOthers $ colocatedWith iid
      choices <- forMaybeM investigators $ \i -> do
        mGainResources <- gainResourcesIfCan i source 2
        mDrawCards <- drawCardsIfCan i source 1
        pure $ (i,) <$> ((:) <$> mGainResources <*> ((:) <$> mDrawCards <*> pure []))

      player <- getPlayer iid
      if null choices
        then error "invalid call"
        else push $ chooseOrRunOne player $ map (uncurry targetLabel) choices
      pure a
    _ -> StrangeSolutionEmpoweringElixir4 <$> runMessage msg attrs
