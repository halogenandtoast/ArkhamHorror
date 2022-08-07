module Arkham.Location.Cards.MiskatonicUniversity
  ( MiskatonicUniversity(..)
  , miskatonicUniversity
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( miskatonicUniversity )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype MiskatonicUniversity = MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversity :: LocationCard MiskatonicUniversity
miskatonicUniversity =
  location MiskatonicUniversity Cards.miskatonicUniversity 4 (PerPlayer 2)

instance HasAbilities MiskatonicUniversity where
  getAbilities (MiskatonicUniversity x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here $ ActionAbility Nothing $ ActionCost 1
    | locationRevealed x
    ]

instance RunMessage MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (Search
        iid
        source
        (InvestigatorTarget iid)
        [fromTopOfDeck 6]
        (CardWithOneOf $ map CardWithTrait [Tome, Spell])
        (DrawFound iid 1)
      )
    _ -> MiskatonicUniversity <$> runMessage msg attrs
