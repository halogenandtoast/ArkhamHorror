module Arkham.Asset.Cards.MadameLabranche (
  madameLabranche,
  MadameLabranche (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype MadameLabranche = MadameLabranche AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madameLabranche :: AssetCard MadameLabranche
madameLabranche = ally MadameLabranche Cards.madameLabranche (2, 2)

instance HasAbilities MadameLabranche where
  getAbilities (MadameLabranche attrs) =
    [ withTooltip "{fast} If you have no cards in your hand, exhaust Madame Labranche: Draw 1 card."
        $ restrictedAbility
          attrs
          1
          (ControlsThis <> InvestigatorExists (You <> HandWith (LengthIs $ EqualTo $ Static 0)))
        $ FastAbility (exhaust attrs)
    , withTooltip "{fast} If you have no resources, exhaust Madame Labranche: Gain 1 resource."
        $ restrictedAbility
          attrs
          2
          (ControlsThis <> InvestigatorExists (You <> InvestigatorWithResources (EqualTo $ Static 0)))
        $ FastAbility (exhaust attrs)
    ]

instance RunMessage MadameLabranche where
  runMessage msg a@(MadameLabranche attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ TakeResources iid 1 (toAbilitySource attrs 2) False
      pure a
    _ -> MadameLabranche <$> runMessage msg attrs
