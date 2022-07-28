module Arkham.Treachery.Cards.SomethingInTheDrinks
  ( SomethingInTheDrinks(..)
  , somethingInTheDrinks
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.Scenario.Types (Field(..))
import Arkham.Treachery.Runner

newtype SomethingInTheDrinks = SomethingInTheDrinks TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingInTheDrinks :: TreacheryCard SomethingInTheDrinks
somethingInTheDrinks =
  treachery SomethingInTheDrinks Cards.somethingInTheDrinks

instance RunMessage SomethingInTheDrinks where
  runMessage msg t@(SomethingInTheDrinks attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      scenarioLogs <- scenarioField ScenarioRemembered
      investigatorIds <- getInvestigatorIds
      t <$ pushAll
        ([ LoseActions iid source 1
         | iid <- investigatorIds
         , HadADrink iid `member` scenarioLogs
         ]
        <> [Continue "Continue"]
        )
    _ -> SomethingInTheDrinks <$> runMessage msg attrs
