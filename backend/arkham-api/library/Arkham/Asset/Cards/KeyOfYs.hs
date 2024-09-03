module Arkham.Asset.Cards.KeyOfYs (keyOfYs, KeyOfYs (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype KeyOfYs = KeyOfYs AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyOfYs :: AssetCard KeyOfYs
keyOfYs = assetWith KeyOfYs Cards.keyOfYs (sanityL ?~ 4)

instance HasModifiersFor KeyOfYs where
  getModifiersFor (InvestigatorTarget iid) (KeyOfYs a) =
    pure $ toModifiers a [AnySkillValue $ assetHorror a | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities KeyOfYs where
  getAbilities (KeyOfYs x) =
    [ restrictedAbility x 1 ControlsThis $ forced $ PlacedCounter #when You AnySource #horror (atLeast 1)
    , restrictedAbility x 2 ControlsThis $ forced $ AssetLeavesPlay #when (be x)
    ]

instance RunMessage KeyOfYs where
  runMessage msg a@(KeyOfYs attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ReassignHorror (toSource iid) (toTarget attrs) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DiscardTopOfDeck iid 10 (toAbilitySource attrs 2) Nothing
      pure a
    _ -> KeyOfYs <$> runMessage msg attrs
