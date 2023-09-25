module Arkham.Asset.Cards.Scrying (
  Scrying (..),
  scrying,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype Scrying = Scrying AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrying :: AssetCard Scrying
scrying = asset Scrying Cards.scrying

instance HasAbilities Scrying where
  getAbilities (Scrying a) =
    [ restrictedAbility a 1 ControlsThis $ actionAbilityWithCost $ assetUseCost a Charge 1 <> exhaust a
    ]

instance RunMessage Scrying where
  runMessage msg a@(Scrying attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      targets <- map InvestigatorTarget <$> getInvestigators
      let source = toAbilitySource attrs 1
      push
        $ chooseOne iid
        $ targetLabels (EncounterDeckTarget : targets)
        $ \target -> only $ lookAt iid source target [(FromTopOfDeck 3, PutBackInAnyOrder)] AnyCard ReturnCards
      pure a
    _ -> Scrying <$> runMessage msg attrs
