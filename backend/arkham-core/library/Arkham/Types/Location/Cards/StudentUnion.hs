module Arkham.Types.Location.Cards.StudentUnion
  ( StudentUnion(..)
  , studentUnion
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (studentUnion)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype StudentUnion = StudentUnion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studentUnion :: LocationCard StudentUnion
studentUnion =
  location StudentUnion Cards.studentUnion 1 (Static 2) Diamond [Plus, Equals]

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

instance LocationRunner env => RunMessage env StudentUnion where
  runMessage msg l@(StudentUnion attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (PlaceLocationMatching $ LocationWithTitle "Dormitories")
    UseCardAbility iid source _ 2 _ | isSource attrs source -> l <$ pushAll
      [ HealDamage (InvestigatorTarget iid) 1
      , HealHorror (InvestigatorTarget iid) 1
      ]
    _ -> StudentUnion <$> runMessage msg attrs
