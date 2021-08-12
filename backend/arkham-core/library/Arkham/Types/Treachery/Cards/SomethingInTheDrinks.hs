module Arkham.Types.Treachery.Cards.SomethingInTheDrinks
  ( SomethingInTheDrinks(..)
  , somethingInTheDrinks
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SomethingInTheDrinks = SomethingInTheDrinks TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingInTheDrinks :: TreacheryCard SomethingInTheDrinks
somethingInTheDrinks =
  treachery SomethingInTheDrinks Cards.somethingInTheDrinks

instance TreacheryRunner env => RunMessage env SomethingInTheDrinks where
  runMessage msg t@(SomethingInTheDrinks attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      scenarioLogs <- getSet ()
      investigatorIds <- getInvestigatorIds
      t <$ pushAll
        ([ LoseActions iid source 1
         | iid <- investigatorIds
         , HadADrink iid `member` scenarioLogs
         ]
        <> [Continue "Continue", Discard $ toTarget attrs]
        )
    _ -> SomethingInTheDrinks <$> runMessage msg attrs
