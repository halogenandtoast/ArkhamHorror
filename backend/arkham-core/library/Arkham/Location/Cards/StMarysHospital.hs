module Arkham.Location.Cards.StMarysHospital
  ( StMarysHospital(..)
  , stMarysHospital
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( stMarysHospital )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype StMarysHospital = StMarysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospital :: LocationCard StMarysHospital
stMarysHospital =
  location StMarysHospital Cards.stMarysHospital 2 (PerPlayer 1)

instance HasAbilities StMarysHospital where
  getAbilities (StMarysHospital x) | locationRevealed x =
    withBaseAbilities x
      $ [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility
              x
              1
              (Here <> InvestigatorExists (You <> InvestigatorWithAnyDamage))
          $ ActionAbility Nothing
          $ ActionCost 1
        ]
  getAbilities (StMarysHospital attrs) = getAbilities attrs

instance RunMessage StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (HealDamage (InvestigatorTarget iid) 3)
    _ -> StMarysHospital <$> runMessage msg attrs
