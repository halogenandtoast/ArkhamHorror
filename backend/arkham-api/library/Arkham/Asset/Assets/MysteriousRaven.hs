module Arkham.Asset.Assets.MysteriousRaven (mysteriousRaven) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype MysteriousRaven = MysteriousRaven AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
        <> HorrorCost (toSource a) YouTarget 1
    ]

instance RunMessage MysteriousRaven where
  runMessage msg a@(MysteriousRaven attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discovery <- discoverAtYourLocation (toAbilitySource attrs 1) 1
      push $ Msg.DiscoverClues iid discovery
      pure a
    _ -> MysteriousRaven <$> runMessage msg attrs
