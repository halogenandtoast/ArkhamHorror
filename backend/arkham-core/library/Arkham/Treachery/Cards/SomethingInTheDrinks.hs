module Arkham.Treachery.Cards.SomethingInTheDrinks (
  SomethingInTheDrinks (..),
  somethingInTheDrinks,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Name
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SomethingInTheDrinks = SomethingInTheDrinks TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

somethingInTheDrinks :: TreacheryCard SomethingInTheDrinks
somethingInTheDrinks =
  treachery SomethingInTheDrinks Cards.somethingInTheDrinks

hadDrinks :: HasGame m => m [InvestigatorId]
hadDrinks = do
  allKeys <- setToList <$> scenarioField ScenarioRemembered
  pure $ flip mapMaybe allKeys $ \case
    HadADrink (Labeled _ iid) -> Just iid
    _ -> Nothing

instance RunMessage SomethingInTheDrinks where
  runMessage msg t@(SomethingInTheDrinks attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- hadDrinks
      pushAll $ [LoseActions iid source 1 | iid <- investigatorIds] <> [Continue "Continue"]
      pure t
    _ -> SomethingInTheDrinks <$> runMessage msg attrs
