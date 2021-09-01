module Arkham.Types.Location.Cards.MiskatonicUniversityMiskatonicMuseum
  ( MiskatonicUniversityMiskatonicMuseum(..)
  , miskatonicUniversityMiskatonicMuseum
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
  (miskatonicUniversityMiskatonicMuseum)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message

newtype MiskatonicUniversityMiskatonicMuseum = MiskatonicUniversityMiskatonicMuseum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityMiskatonicMuseum
  :: LocationCard MiskatonicUniversityMiskatonicMuseum
miskatonicUniversityMiskatonicMuseum = location
  MiskatonicUniversityMiskatonicMuseum
  Cards.miskatonicUniversityMiskatonicMuseum
  3
  (PerPlayer 1)
  Diamond
  [T, Plus, Circle, Square]

instance HasAbilities env MiskatonicUniversityMiskatonicMuseum where
  getAbilities iid window (MiskatonicUniversityMiskatonicMuseum attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1 (ActionAbility Nothing $ ActionCost 1)
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance (LocationRunner env) => RunMessage env MiskatonicUniversityMiskatonicMuseum where
  runMessage msg l@(MiskatonicUniversityMiskatonicMuseum attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [InvestigatorAssignDamage iid source DamageAny 0 2, GainClues iid 1]
    _ -> MiskatonicUniversityMiskatonicMuseum <$> runMessage msg attrs
