module Arkham.Act.Cards.TheIsleOfOriab (TheIsleOfOriab (..), theIsleOfOriab) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Types
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheSearchForKadath.Helpers
import Arkham.Trait (Trait (Port))

newtype TheIsleOfOriab = TheIsleOfOriab ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theIsleOfOriab :: ActCard TheIsleOfOriab
theIsleOfOriab = act (2, A) TheIsleOfOriab Cards.theIsleOfOriab Nothing

instance HasAbilities TheIsleOfOriab where
  getAbilities (TheIsleOfOriab x) =
    [ restrictedAbility x 1 (EachUndefeatedInvestigator $ InvestigatorAt $ LocationWithTrait Port)
        $ Objective
        $ ReactionAbility (RoundEnds #when) Free
    , restrictedAbility x 2 (HasScenarioCount SignOfTheGods $ atLeast 10)
        $ Objective
        $ ForcedAbility AnyWindow
    ]

toOption :: Region -> UI Message
toOption = \case
  Oriab -> error "Not possible"
  Mnar ->
    Label
      "Visit the ancient land of Mnar to the west. Resolve Mnar Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON Mnar]
  ForbiddenLands ->
    Label
      "Visit the Forbidden Lands to the north. Resolve Forbidden Lands Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON ForbiddenLands]
  TimelessRealm ->
    Label
      "Visit the kingdom of the Timeless Realm to the east. Resolve Timeless Realm Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON TimelessRealm]

instance RunMessage TheIsleOfOriab where
  runMessage msg a@(TheIsleOfOriab attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      n <- scenarioFieldMap ScenarioMeta toResult
      let availableRegions = filter (`notElem` regions n) [Mnar, ForbiddenLands, TimelessRealm]
      if null availableRegions
        then push R1
        else push $ chooseOrRunOne lead $ map toOption availableRegions
      push $ ShuffleEncounterDiscardBackIn
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R1
      pure a
    _ -> TheIsleOfOriab <$> runMessage msg attrs
