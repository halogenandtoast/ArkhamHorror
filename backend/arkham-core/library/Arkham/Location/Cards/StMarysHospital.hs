module Arkham.Location.Cards.StMarysHospital
  ( StMarysHospital(..)
  , stMarysHospital
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message

newtype StMarysHospital = StMarysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospital :: LocationCard StMarysHospital
stMarysHospital =
  location StMarysHospital Cards.stMarysHospital 2 (PerPlayer 1)

instance HasAbilities StMarysHospital where
  getAbilities (StMarysHospital x) | locationRevealed x = withBaseAbilities
    x
    [ limitedAbility (PlayerLimit PerGame 1)
      $ restrictedAbility
          x
          1
          (Here <> InvestigatorExists
            (HealableInvestigator (toSource x) DamageType You)
          )
      $ ActionAbility Nothing
      $ ActionCost 1
    ]
  getAbilities (StMarysHospital attrs) = getAbilities attrs

instance RunMessage StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ HealDamage (InvestigatorTarget iid) (toSource attrs) 3
      pure l
    _ -> StMarysHospital <$> runMessage msg attrs
