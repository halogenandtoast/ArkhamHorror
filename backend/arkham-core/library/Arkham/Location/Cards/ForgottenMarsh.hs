module Arkham.Location.Cards.ForgottenMarsh where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (forgottenMarsh)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype ForgottenMarsh = ForgottenMarsh LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgottenMarsh :: LocationCard ForgottenMarsh
forgottenMarsh = location
  ForgottenMarsh
  Cards.forgottenMarsh
  2
  (Static 0)
  Diamond
  [Moon, Square, Triangle, Hourglass]

instance HasAbilities ForgottenMarsh where
  getAbilities (ForgottenMarsh attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1
        $ ForcedAbility
        $ Leaves Timing.When You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env ForgottenMarsh where
  runMessage msg l@(ForgottenMarsh attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (SpendResources iid 2)
    _ -> ForgottenMarsh <$> runMessage msg attrs
