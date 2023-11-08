module Arkham.Location.Cards.MiskatonicUniversityMiskatonicMuseum (
  MiskatonicUniversityMiskatonicMuseum (..),
  miskatonicUniversityMiskatonicMuseum,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (
  miskatonicUniversityMiskatonicMuseum,
 )
import Arkham.Location.Runner

newtype MiskatonicUniversityMiskatonicMuseum = MiskatonicUniversityMiskatonicMuseum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityMiskatonicMuseum
  :: LocationCard MiskatonicUniversityMiskatonicMuseum
miskatonicUniversityMiskatonicMuseum =
  location
    MiskatonicUniversityMiskatonicMuseum
    Cards.miskatonicUniversityMiskatonicMuseum
    3
    (PerPlayer 1)

instance HasAbilities MiskatonicUniversityMiskatonicMuseum where
  getAbilities (MiskatonicUniversityMiskatonicMuseum attrs) =
    withRevealedAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ restrictedAbility attrs 1 Here
            $ ActionAbility []
            $ ActionCost 1
        ]

instance RunMessage MiskatonicUniversityMiskatonicMuseum where
  runMessage msg l@(MiskatonicUniversityMiskatonicMuseum attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [InvestigatorAssignDamage iid source DamageAny 0 2, GainClues iid (toAbilitySource attrs 1) 1]
      pure l
    _ -> MiskatonicUniversityMiskatonicMuseum <$> runMessage msg attrs
