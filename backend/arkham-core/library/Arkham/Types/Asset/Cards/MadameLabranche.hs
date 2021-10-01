module Arkham.Types.Asset.Cards.MadameLabranche
  ( madameLabranche
  , MadameLabranche(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype MadameLabranche = MadameLabranche AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madameLabranche :: AssetCard MadameLabranche
madameLabranche = ally MadameLabranche Cards.madameLabranche (2, 2)

instance HasAbilities MadameLabranche where
  getAbilities (MadameLabranche attrs) =
    [ restrictedAbility
        attrs
        1
        (OwnsThis <> InvestigatorExists
          (You <> HandWith (LengthIs $ EqualTo $ Static 0))
        )
      $ FastAbility
      $ ExhaustCost
      $ toTarget attrs
    , restrictedAbility
        attrs
        2
        (OwnsThis <> InvestigatorExists
          (You <> InvestigatorWithResources (EqualTo $ Static 0))
        )
      $ FastAbility
      $ ExhaustCost
      $ toTarget attrs
    ]

instance AssetRunner env => RunMessage env MadameLabranche where
  runMessage msg a@(MadameLabranche attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ push (TakeResources iid 1 False)
    _ -> MadameLabranche <$> runMessage msg attrs
