module Arkham.Treachery.Cards.TheBellTolls (
  theBellTolls,
  TheBellTolls (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheBellTolls = TheBellTolls TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBellTolls :: TreacheryCard TheBellTolls
theBellTolls = treachery TheBellTolls Cards.theBellTolls

instance RunMessage TheBellTolls where
  runMessage msg t@(TheBellTolls attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ InvestigatorKilled source iid
      pure t
    _ -> TheBellTolls <$> runMessage msg attrs
