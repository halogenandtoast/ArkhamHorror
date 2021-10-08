module Arkham.Types.Location.Cards.MessHall
  ( messHall
  , MessHall(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype MessHall = MessHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

messHall :: LocationCard MessHall
messHall =
  location MessHall Cards.messHall 2 (PerPlayer 2) Triangle [Circle, Square]

instance HasAbilities MessHall where
  getAbilities (MessHall attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ForcedAbility
      $ SkillTestResult Timing.After You (WhileInvestigating YourLocation)
      $ SuccessResult AnyValue
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env MessHall where
  runMessage msg l@(MessHall attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (ChooseAndDiscardCard iid)
    _ -> MessHall <$> runMessage msg attrs
