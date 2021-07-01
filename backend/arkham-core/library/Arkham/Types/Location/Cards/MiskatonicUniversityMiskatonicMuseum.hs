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
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype MiskatonicUniversityMiskatonicMuseum = MiskatonicUniversityMiskatonicMuseum LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityMiskatonicMuseum
  :: LocationId -> MiskatonicUniversityMiskatonicMuseum
miskatonicUniversityMiskatonicMuseum =
  MiskatonicUniversityMiskatonicMuseum . baseAttrs
    Cards.miskatonicUniversityMiskatonicMuseum
    3
    (PerPlayer 1)
    Diamond
    [T, Plus, Circle, Square]

instance HasModifiersFor env MiskatonicUniversityMiskatonicMuseum where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerGame 1 }
 where
  base = mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1)

instance ActionRunner env => HasActions env MiskatonicUniversityMiskatonicMuseum where
  getActions iid NonFast (MiskatonicUniversityMiskatonicMuseum attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (MiskatonicUniversityMiskatonicMuseum attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env MiskatonicUniversityMiskatonicMuseum where
  runMessage msg l@(MiskatonicUniversityMiskatonicMuseum attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages
        [InvestigatorAssignDamage iid source DamageAny 0 2, GainClues iid 1]
    _ -> MiskatonicUniversityMiskatonicMuseum <$> runMessage msg attrs
