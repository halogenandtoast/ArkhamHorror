module Arkham.Location.Cards.BrokenPassage
  ( brokenPassage
  , BrokenPassage(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype BrokenPassage = BrokenPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenPassage :: LocationCard BrokenPassage
brokenPassage =
  symbolLabel $ location BrokenPassage Cards.brokenPassage 3 (Static 0)

instance HasAbilities BrokenPassage where
  getAbilities (BrokenPassage attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Here <> InvestigatorExists
          (You <> NotInvestigator (InvestigatorWithSupply Pickaxe))
        )
      $ ForcedAbility
      $ AttemptExplore Timing.When You
    ]

instance RunMessage BrokenPassage where
  runMessage msg l@(BrokenPassage attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceDoom (toTarget attrs) 1
      pure l
    _ -> BrokenPassage <$> runMessage msg attrs
