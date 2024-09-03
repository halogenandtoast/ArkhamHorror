module Arkham.Act.Cards.TheDoomThatCameBefore (TheDoomThatCameBefore (..), theDoomThatCameBefore) where

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

newtype TheDoomThatCameBefore = TheDoomThatCameBefore ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomThatCameBefore :: ActCard TheDoomThatCameBefore
theDoomThatCameBefore = act (2, A) TheDoomThatCameBefore Cards.theDoomThatCameBefore Nothing

instance HasAbilities TheDoomThatCameBefore where
  getAbilities (TheDoomThatCameBefore x) =
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
  Mnar -> error "Not possible"
  ForbiddenLands ->
    Label
      "Visit the Forbidden Lands to the north. Resolve Forbidden Lands Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON ForbiddenLands]
  TimelessRealm ->
    Label
      "Visit the kingdom of the Timeless Realm to the east. Resolve Timeless Realm Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON TimelessRealm]

instance RunMessage TheDoomThatCameBefore where
  runMessage msg a@(TheDoomThatCameBefore attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      n <- scenarioFieldMap ScenarioMeta toResult
      let availableRegions = filter (`notElem` regions n) [Oriab, ForbiddenLands, TimelessRealm]
      if null availableRegions
        then push R1
        else push $ chooseOrRunOne lead $ map toOption availableRegions
      push $ ShuffleEncounterDiscardBackIn
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R1
      pure a
    _ -> TheDoomThatCameBefore <$> runMessage msg attrs
