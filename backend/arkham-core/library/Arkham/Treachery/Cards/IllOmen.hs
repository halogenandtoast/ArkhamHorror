module Arkham.Treachery.Cards.IllOmen
  ( illOmen
  , IllOmen(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype IllOmen = IllOmen TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illOmen :: TreacheryCard IllOmen
illOmen = treachery IllOmen Cards.illOmen

instance RunMessage IllOmen where
  runMessage msg t@(IllOmen attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> IllOmen <$> runMessage msg attrs
