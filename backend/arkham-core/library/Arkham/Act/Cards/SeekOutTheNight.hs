module Arkham.Act.Cards.SeekOutTheNight (SeekOutTheNight (..), seekOutTheNight) where

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

newtype SeekOutTheNight = SeekOutTheNight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

seekOutTheNight :: ActCard SeekOutTheNight
seekOutTheNight = act (2, A) SeekOutTheNight Cards.seekOutTheNight Nothing

instance HasAbilities SeekOutTheNight where
  getAbilities (SeekOutTheNight x) =
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
  ForbiddenLands -> error "Not possible"
  TimelessRealm ->
    Label
      "Visit the kingdom of the Timeless Realm to the east. Resolve Timeless Realm Setup in the Campaign Guide."
      [SetScenarioMeta $ toJSON TimelessRealm]

instance RunMessage SeekOutTheNight where
  runMessage msg a@(SeekOutTheNight attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> runQueueT $ do
      lead <- getLeadPlayer
      n <- scenarioFieldMap ScenarioMeta toResult
      push $ ShuffleEncounterDiscardBackIn
      let availableRegions = filter (`notElem` regions n) [Oriab, Mnar, TimelessRealm]
      if null availableRegions
        then push R1
        else push $ chooseOrRunOne lead $ map toOption availableRegions
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R1
      pure a
    _ -> SeekOutTheNight <$> runMessage msg attrs
