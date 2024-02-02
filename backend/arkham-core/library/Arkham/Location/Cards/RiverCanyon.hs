module Arkham.Location.Cards.RiverCanyon (
  riverCanyon,
  RiverCanyon (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype RiverCanyon = RiverCanyon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

riverCanyon :: LocationCard RiverCanyon
riverCanyon = location RiverCanyon Cards.riverCanyon 4 (PerPlayer 1)

instance HasAbilities RiverCanyon where
  getAbilities (RiverCanyon attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility
            attrs
            1
            (Here <> InvestigatorExists (HealableInvestigator (toSource attrs) DamageType You))
          $ ActionAbility []
          $ ActionCost 1
      ]

instance RunMessage RiverCanyon where
  runMessage msg l@(RiverCanyon attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      hasCanteen <- getHasSupply iid Canteen
      push $ HealDamage (InvestigatorTarget iid) (toSource attrs) (if hasCanteen then 3 else 1)
      pure l
    _ -> RiverCanyon <$> runMessage msg attrs
