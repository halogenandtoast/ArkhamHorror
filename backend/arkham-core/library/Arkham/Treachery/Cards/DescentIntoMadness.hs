module Arkham.Treachery.Cards.DescentIntoMadness (
  descentIntoMadness,
  DescentIntoMadness (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DescentIntoMadness = DescentIntoMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

descentIntoMadness :: TreacheryCard DescentIntoMadness
descentIntoMadness = treachery DescentIntoMadness Cards.descentIntoMadness

instance RunMessage DescentIntoMadness where
  runMessage msg t@(DescentIntoMadness attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horrorCount <- field InvestigatorHorror iid
      t <$ when (horrorCount >= 3) (push $ LoseActions iid source 1)
    _ -> DescentIntoMadness <$> runMessage msg attrs
