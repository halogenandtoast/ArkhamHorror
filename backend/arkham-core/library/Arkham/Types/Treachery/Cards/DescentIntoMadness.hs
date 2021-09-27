module Arkham.Types.Treachery.Cards.DescentIntoMadness
  ( descentIntoMadness
  , DescentIntoMadness(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype DescentIntoMadness = DescentIntoMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentIntoMadness :: TreacheryCard DescentIntoMadness
descentIntoMadness = treachery DescentIntoMadness Cards.descentIntoMadness

instance TreacheryRunner env => RunMessage env DescentIntoMadness where
  runMessage msg t@(DescentIntoMadness attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horrorCount <- unHorrorCount <$> getCount iid
      t <$ when (horrorCount >= 3) (push $ LoseActions iid source 1)
    _ -> DescentIntoMadness <$> runMessage msg attrs
