module Arkham.Types.Location.Cards.Bedroom where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (bedroom)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype Bedroom = Bedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroom :: LocationCard Bedroom
bedroom = location Bedroom Cards.bedroom 2 (PerPlayer 1) Heart [T]

instance HasAbilities env Bedroom where
  getAbilities i window (Bedroom attrs) =
    withBaseAbilities i window attrs $ pure
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
