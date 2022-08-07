module Arkham.Asset.Cards.MadameLabranche
  ( madameLabranche
  , MadameLabranche(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher

newtype MadameLabranche = MadameLabranche AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madameLabranche :: AssetCard MadameLabranche
madameLabranche = ally MadameLabranche Cards.madameLabranche (2, 2)

instance HasAbilities MadameLabranche where
  getAbilities (MadameLabranche attrs) =
    [ withTooltip
        "{fast} If you have no cards in your hand, exhaust Madame Labranche: Draw 1 card."
      $ restrictedAbility
          attrs
          1
          (ControlsThis <> InvestigatorExists
            (You <> HandWith (LengthIs $ EqualTo $ Static 0))
          )
      $ FastAbility
      $ ExhaustCost
      $ toTarget attrs
    , withTooltip
        "{fast} If you have no resources, exhaust Madame Labranche: Gain 1 resource."
      $ restrictedAbility
          attrs
          2
          (ControlsThis <> InvestigatorExists
            (You <> InvestigatorWithResources (EqualTo $ Static 0))
          )
      $ FastAbility
      $ ExhaustCost
      $ toTarget attrs
    ]

instance RunMessage MadameLabranche where
  runMessage msg a@(MadameLabranche attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ push (TakeResources iid 1 False)
    _ -> MadameLabranche <$> runMessage msg attrs
