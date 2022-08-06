module Arkham.Location.Cards.StudentUnion
  ( StudentUnion(..)
  , studentUnion
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( studentUnion )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype StudentUnion = StudentUnion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studentUnion :: LocationCard StudentUnion
studentUnion = location StudentUnion Cards.studentUnion 1 (Static 2)

instance HasAbilities StudentUnion where
  getAbilities (StudentUnion attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.After Anyone
        $ LocationWithId
        $ toId attrs
        , restrictedAbility attrs 2 Here $ ActionAbility Nothing $ ActionCost 2
        ]
      else []

instance RunMessage StudentUnion where
  runMessage msg l@(StudentUnion attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (PlaceLocationMatching $ CardWithTitle "Dormitories")
    UseCardAbility iid source _ 2 _ | isSource attrs source -> l <$ pushAll
      [ HealDamage (InvestigatorTarget iid) 1
      , HealHorror (InvestigatorTarget iid) 1
      ]
    _ -> StudentUnion <$> runMessage msg attrs
