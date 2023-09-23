module Arkham.Location.Cards.StMarysHospital (
  StMarysHospital (..),
  stMarysHospital,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype StMarysHospital = StMarysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospital :: LocationCard StMarysHospital
stMarysHospital = location StMarysHospital Cards.stMarysHospital 2 (PerPlayer 1)

instance HasAbilities StMarysHospital where
  getAbilities (StMarysHospital x) =
    withRevealedAbilities x
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ withCriteria (mkAbility x 1 (ActionAbility Nothing $ ActionCost 1))
            $ Here
            <> InvestigatorExists (HealableInvestigator (toSource x) DamageType You)
        ]

instance RunMessage StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ HealDamage (toTarget iid) (toAbilitySource attrs 1) 3
      pure l
    _ -> StMarysHospital <$> runMessage msg attrs
