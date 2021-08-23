module Arkham.Types.Location.Cards.MiskatonicUniversity
  ( MiskatonicUniversity(..)
  , miskatonicUniversity
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (miskatonicUniversity)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype MiskatonicUniversity = MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversity :: LocationCard MiskatonicUniversity
miskatonicUniversity = location
  MiskatonicUniversity
  Cards.miskatonicUniversity
  4
  (PerPlayer 2)
  Diamond
  [T, Plus, Circle, Square]

instance HasAbilities env MiskatonicUniversity where
  getAbilities iid window (MiskatonicUniversity x) | locationRevealed x =
    withBaseAbilities iid window x
      $ pure [restrictedAbility x 1 Here $ ActionAbility Nothing $ ActionCost 1]
  getAbilities iid window (MiskatonicUniversity x) = getAbilities iid window x

instance LocationRunner env => RunMessage env MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (SearchTopOfDeck
        iid
        source
        (InvestigatorTarget iid)
        6
        [Tome, Spell]
        (ShuffleBackIn $ DrawFound iid)
      )
    _ -> MiskatonicUniversity <$> runMessage msg attrs
