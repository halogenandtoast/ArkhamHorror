module Arkham.Types.Location.Cards.MiskatonicUniversity
  ( MiskatonicUniversity(..)
  , miskatonicUniversity
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (miskatonicUniversity)
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
import Arkham.Types.Zone

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

instance HasAbilities MiskatonicUniversity where
  getAbilities (MiskatonicUniversity x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here $ ActionAbility Nothing $ ActionCost 1
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (Search
        iid
        source
        (InvestigatorTarget iid)
        (FromTopOfDeck 6)
        [Tome, Spell]
        (ShuffleBackIn $ DrawFound iid)
      )
    _ -> MiskatonicUniversity <$> runMessage msg attrs
