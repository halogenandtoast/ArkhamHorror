module Arkham.Asset.Cards.ArmorOfArdennes5
  ( armorOfArdennes5
  , ArmorOfArdennes5(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window qualified as Window
import Arkham.Window (Window(..))

newtype ArmorOfArdennes5 = ArmorOfArdennes5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armorOfArdennes5 :: AssetCard ArmorOfArdennes5
armorOfArdennes5 =
  assetWith ArmorOfArdennes5 Cards.armorOfArdennes5 (healthL ?~ 4)

instance HasAbilities ArmorOfArdennes5 where
  getAbilities (ArmorOfArdennes5 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (AssetDealtDamage Timing.When (SourceIsCancelable AnySource) $ AssetWithId $ toId a)
        (ExhaustCost $ toTarget a)
    ]

instance RunMessage ArmorOfArdennes5 where
  runMessage msg (ArmorOfArdennes5 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      push ignoreWindow
      pure . ArmorOfArdennes5 $ attrs & damageL -~ 1
    _ -> ArmorOfArdennes5 <$> runMessage msg attrs
