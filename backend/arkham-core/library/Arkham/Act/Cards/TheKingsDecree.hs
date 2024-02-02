module Arkham.Act.Cards.TheKingsDecree (TheKingsDecree (..), theKingsDecree) where

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

newtype TheKingsDecree = TheKingsDecree ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theKingsDecree :: ActCard TheKingsDecree
theKingsDecree = act (2, A) TheKingsDecree Cards.theKingsDecree Nothing

instance HasAbilities TheKingsDecree where
  getAbilities (TheKingsDecree x) =
    [ restrictedAbility x 1 (EachUndefeatedInvestigator $ InvestigatorAt $ LocationWithTrait Port)
        $ Objective
        $ ReactionAbility (RoundEnds #when) Free
    , restrictedAbility x 2 (HasScenarioCount SignOfTheGods $ atLeast 10)
        $ Objective
        $ ForcedAbility AnyWindow
    ]

toOption :: Region -> UI Message
toOption = \case
  Oriab ->
    Label
      "Visit the isle of Oriab to the south. Resolve Oriab Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON Oriab]
  Mnar ->
    Label
      "Visit the ancient land of Mnar to the west. Resolve Mnar Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON Mnar]
  ForbiddenLands ->
    Label
      "Visit the Forbidden Lands to the north. Resolve Forbidden Lands Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON ForbiddenLands]
  TimelessRealm -> error "Not possible"

instance RunMessage TheKingsDecree where
  runMessage msg a@(TheKingsDecree attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      n <- scenarioFieldMap ScenarioMeta toResult
      let availableRegions = filter (`notElem` regions n) [Oriab, Mnar, ForbiddenLands]
      if null availableRegions
        then push R1
        else push $ chooseOrRunOne lead $ map toOption availableRegions
      push $ ShuffleEncounterDiscardBackIn
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R1
      pure a
    _ -> TheKingsDecree <$> runMessage msg attrs
