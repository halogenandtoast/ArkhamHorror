module Arkham.Location.Cards.MiskatonicUniversityMiskatonicMuseum
  ( MiskatonicUniversityMiskatonicMuseum(..)
  , miskatonicUniversityMiskatonicMuseum
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
  ( miskatonicUniversityMiskatonicMuseum )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message

newtype MiskatonicUniversityMiskatonicMuseum = MiskatonicUniversityMiskatonicMuseum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityMiskatonicMuseum
  :: LocationCard MiskatonicUniversityMiskatonicMuseum
miskatonicUniversityMiskatonicMuseum = location
  MiskatonicUniversityMiskatonicMuseum
  Cards.miskatonicUniversityMiskatonicMuseum
  3
  (PerPlayer 1)

instance HasAbilities MiskatonicUniversityMiskatonicMuseum where
  getAbilities (MiskatonicUniversityMiskatonicMuseum attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
            & (abilityLimitL .~ PlayerLimit PerGame 1)
        | locationRevealed attrs
        ]

instance RunMessage MiskatonicUniversityMiskatonicMuseum where
  runMessage msg l@(MiskatonicUniversityMiskatonicMuseum attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ pushAll
      [InvestigatorAssignDamage iid source DamageAny 0 2, GainClues iid 1]
    _ -> MiskatonicUniversityMiskatonicMuseum <$> runMessage msg attrs
