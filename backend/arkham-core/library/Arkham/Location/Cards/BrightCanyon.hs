module Arkham.Location.Cards.BrightCanyon (brightCanyon, BrightCanyon (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Window (enters)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

newtype BrightCanyon = BrightCanyon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

brightCanyon :: LocationCard BrightCanyon
brightCanyon = symbolLabel $ location BrightCanyon Cards.brightCanyon 2 (PerPlayer 2)

instance HasAbilities BrightCanyon where
  getAbilities (BrightCanyon attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 (you $ have Treacheries.poisoned)
          $ forced
          $ enters #after You attrs
      , groupLimit PerDepthLevel
          $ restrictedAbility attrs 2 (Here <> cluesOnThis 1 <> you (have Binoculars)) actionAbility
      ]

instance RunMessage BrightCanyon where
  runMessage msg l@(BrightCanyon attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ assignDamage iid (toAbilitySource attrs 1) 1
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 2) 2 Nothing
      pure l
    _ -> BrightCanyon <$> runMessage msg attrs
