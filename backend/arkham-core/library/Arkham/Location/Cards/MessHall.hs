module Arkham.Location.Cards.MessHall
  ( messHall
  , MessHall(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype MessHall = MessHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

messHall :: LocationCard MessHall
messHall = location MessHall Cards.messHall 2 (PerPlayer 2)

instance HasAbilities MessHall where
  getAbilities (MessHall attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ForcedAbility
      $ SkillTestResult Timing.After You (WhileInvestigating YourLocation)
      $ SuccessResult AnyValue
    | locationRevealed attrs
    ]

instance RunMessage MessHall where
  runMessage msg l@(MessHall attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ toMessage $ chooseAndDiscardCard iid (toAbilitySource attrs 1)
      pure l
    _ -> MessHall <$> runMessage msg attrs
