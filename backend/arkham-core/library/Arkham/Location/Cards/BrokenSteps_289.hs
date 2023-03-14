module Arkham.Location.Cards.BrokenSteps_289
  ( brokenSteps_289
  , BrokenSteps_289(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype BrokenSteps_289 = BrokenSteps_289 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenSteps_289 :: LocationCard BrokenSteps_289
brokenSteps_289 = location BrokenSteps_289 Cards.brokenSteps_289 4 (Static 0)

instance HasAbilities BrokenSteps_289 where
  getAbilities (BrokenSteps_289 a) =
    withBaseAbilities a
      $ [ mkAbility a 1
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId
          $ toId a
        ]

instance RunMessage BrokenSteps_289 where
  runMessage msg l@(BrokenSteps_289 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      actionsRemaining <- field InvestigatorRemainingActions iid
      mOmenCard <-
        find (`cardMatch` (CardWithTrait Omen <> CardWithType TreacheryType))
          <$> scenarioField ScenarioDiscard
      let
        choices =
          [ Label "Lose 1 Action" [LoseActions iid source 1]
          | actionsRemaining > 0
          ]
          <> [ Label
                 "Draw the topmost omen treachery in the encounter discard pile"
                 [FindAndDrawEncounterCard iid (CardWithId $ toCardId c) True]
             | c <- maybeToList mOmenCard
             ]
      unless (null choices) $ push $ chooseOne iid choices
      pure l
    _ -> BrokenSteps_289 <$> runMessage msg attrs
