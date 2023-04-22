module Arkham.Location.Cards.ForgottenMarsh where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( forgottenMarsh )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ForgottenMarsh = ForgottenMarsh LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgottenMarsh :: LocationCard ForgottenMarsh
forgottenMarsh = location ForgottenMarsh Cards.forgottenMarsh 2 (Static 0)

instance HasAbilities ForgottenMarsh where
  getAbilities (ForgottenMarsh attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ Leaves Timing.When You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage ForgottenMarsh where
  runMessage msg l@(ForgottenMarsh attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (SpendResources iid 2)
    _ -> ForgottenMarsh <$> runMessage msg attrs
