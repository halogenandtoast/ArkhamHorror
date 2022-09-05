module Arkham.Asset.Cards.MysteriousRaven
  ( mysteriousRaven
  , MysteriousRaven(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target

newtype MysteriousRaven = MysteriousRaven AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousRaven :: AssetCard MysteriousRaven
mysteriousRaven =
  assetWith MysteriousRaven Cards.mysteriousRaven (sanityL ?~ 1)

instance HasAbilities MysteriousRaven where
  getAbilities (MysteriousRaven a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> ClueOnLocation <> InvestigatorExists
            (You <> InvestigatorCanDiscoverCluesAt YourLocation)
          )
        $ FastAbility
        $ DiscardCost (toTarget a)
        <> DamageCost (toSource a) YouTarget 1
    ]

instance RunMessage MysteriousRaven where
  runMessage msg a@(MysteriousRaven attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing
      pure a
    _ -> MysteriousRaven <$> runMessage msg attrs
