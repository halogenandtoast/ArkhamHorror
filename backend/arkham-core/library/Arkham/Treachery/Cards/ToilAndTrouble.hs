module Arkham.Treachery.Cards.ToilAndTrouble (
  toilAndTrouble,
  ToilAndTrouble (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Power))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ToilAndTrouble = ToilAndTrouble TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toilAndTrouble :: TreacheryCard ToilAndTrouble
toilAndTrouble = treachery ToilAndTrouble Cards.toilAndTrouble

instance RunMessage ToilAndTrouble where
  runMessage msg t@(ToilAndTrouble attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      discard <- scenarioField ScenarioDiscard
      let mPowerTreachery = find (`cardMatch` CardWithTrait Power) discard
      location <- getJustLocation iid
      push
        $ chooseOrRunOne iid
        $ [ Label
            "Resolve the revelation ability on the topmost Power treachery in the encounter discard pile"
            [InvestigatorDrewEncounterCard iid powerTreachery]
          | powerTreachery <- maybeToList mPowerTreachery
          ]
        <> [Label "Resolve an incursion at your location" [Incursion location]]
      pure t
    _ -> ToilAndTrouble <$> runMessage msg attrs
