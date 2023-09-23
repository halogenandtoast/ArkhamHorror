module Arkham.Location.Cards.BrightCanyon (
  brightCanyon,
  BrightCanyon (..),
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
import Arkham.Treachery.Cards qualified as Treacheries

newtype BrightCanyon = BrightCanyon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brightCanyon :: LocationCard BrightCanyon
brightCanyon =
  symbolLabel $ location BrightCanyon Cards.brightCanyon 2 (PerPlayer 2)

instance HasAbilities BrightCanyon where
  getAbilities (BrightCanyon attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          ( InvestigatorExists
              $ You
              <> HasMatchingTreachery
                (treacheryIs Treacheries.poisoned)
          )
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId
          $ toId attrs
      , limitedAbility (GroupLimit PerDepthLevel 1)
          $ restrictedAbility
            attrs
            2
            ( CluesOnThis (AtLeast $ Static 1)
                <> InvestigatorExists (You <> InvestigatorWithSupply Binoculars)
            )
          $ ActionAbility Nothing
          $ ActionCost 1
      ]

instance RunMessage BrightCanyon where
  runMessage msg l@(BrightCanyon attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 2) 2 Nothing
      pure l
    _ -> BrightCanyon <$> runMessage msg attrs
