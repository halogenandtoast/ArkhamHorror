module Arkham.Location.Cards.Bedroom where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (bedroom)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype Bedroom = Bedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroom :: LocationCard Bedroom
bedroom = location Bedroom Cards.bedroom 2 (PerPlayer 1) Heart [T]

instance HasAbilities Bedroom where
  getAbilities (Bedroom attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1
        $ ForcedAbility
        $ SkillTestResult
            Timing.After
            You
            (WhileInvestigating $ LocationWithId $ toId attrs)
        $ FailureResult AnyValue
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env Bedroom where
  runMessage msg l@(Bedroom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (RandomDiscard iid)
    _ -> Bedroom <$> runMessage msg attrs
