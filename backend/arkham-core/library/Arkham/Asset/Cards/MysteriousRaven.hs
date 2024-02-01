module Arkham.Asset.Cards.MysteriousRaven (
  mysteriousRaven,
  MysteriousRaven (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Matcher

newtype MysteriousRaven = MysteriousRaven AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

mysteriousRaven :: AssetCard MysteriousRaven
mysteriousRaven =
  assetWith MysteriousRaven Cards.mysteriousRaven (sanityL ?~ 1)

instance HasAbilities MysteriousRaven where
  getAbilities (MysteriousRaven a) =
    [ controlledAbility
        a
        1
        (ClueOnLocation <> exists (You <> InvestigatorCanDiscoverCluesAt YourLocation))
        $ FastAbility
        $ DiscardCost FromPlay (toTarget a)
        <> DamageCost (toSource a) YouTarget 1
    ]

instance RunMessage MysteriousRaven where
  runMessage msg a@(MysteriousRaven attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ discoverAtYourLocation iid (toAbilitySource attrs 1) 1
      pure a
    _ -> MysteriousRaven <$> runMessage msg attrs
