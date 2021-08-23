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
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

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

ability :: LocationAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerGame 1 }
 where
  base = mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1)

instance HasAbilities env MiskatonicUniversityMiskatonicMuseum where
  getAbilities iid window@(Window Timing.When NonFast) (MiskatonicUniversityMiskatonicMuseum attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window (MiskatonicUniversityMiskatonicMuseum attrs) =
    getAbilities iid window attrs

instance (LocationRunner env) => RunMessage env MiskatonicUniversityMiskatonicMuseum where
  runMessage msg l@(MiskatonicUniversityMiskatonicMuseum attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [InvestigatorAssignDamage iid source DamageAny 0 2, GainClues iid 1]
    _ -> MiskatonicUniversityMiskatonicMuseum <$> runMessage msg attrs
