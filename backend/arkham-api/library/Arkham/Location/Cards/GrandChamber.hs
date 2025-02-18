module Arkham.Location.Cards.GrandChamber (grandChamber) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Timing qualified as Timing

newtype GrandChamber = GrandChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandChamber :: LocationCard GrandChamber
grandChamber =
  locationWith
    GrandChamber
    Cards.grandChamber
    2
    (PerPlayer 1)
    (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities GrandChamber where
  getAbilities (GrandChamber attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ SkillTestResult
            Timing.When
            You
            (WhileInvestigating $ LocationWithId $ toId attrs)
          $ ResultOneOf
            [FailureResult AnyValue, SuccessResult $ LessThan $ Static 2]
      ]

instance RunMessage GrandChamber where
  runMessage msg l@(GrandChamber attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ FlipClues (toTarget attrs) 1
      pure l
    _ -> GrandChamber <$> runMessage msg attrs
