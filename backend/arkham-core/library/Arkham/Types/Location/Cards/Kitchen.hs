module Arkham.Types.Location.Cards.Kitchen
  ( kitchen
  , Kitchen(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = location Kitchen Cards.kitchen 2 (PerPlayer 1) Square [Triangle]

instance HasAbilities Kitchen where
  getAbilities (Kitchen attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance HasModifiersFor env Kitchen where
  getModifiersFor _ (LocationTarget lid) (Kitchen attrs) | lid == toId attrs =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage env Kitchen where
  runMessage msg l@(Kitchen attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ push (Remember SetAFireInTheKitchen)
    _ -> Kitchen <$> runMessage msg attrs
