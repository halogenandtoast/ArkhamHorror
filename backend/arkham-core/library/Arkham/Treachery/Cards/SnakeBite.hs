module Arkham.Treachery.Cards.SnakeBite
  ( snakeBite
  , SnakeBite(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype SnakeBite = SnakeBite TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snakeBite :: TreacheryCard SnakeBite
snakeBite = treachery SnakeBite Cards.snakeBite

instance RunMessage SnakeBite where
  runMessage msg t@(SnakeBite attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> SnakeBite <$> runMessage msg attrs
