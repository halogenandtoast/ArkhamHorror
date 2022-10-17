module Arkham.Treachery.Cards.PoisonousSpores
  ( poisonousSpores
  , PoisonousSpores(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype PoisonousSpores = PoisonousSpores TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisonousSpores :: TreacheryCard PoisonousSpores
poisonousSpores = treachery PoisonousSpores Cards.poisonousSpores

instance RunMessage PoisonousSpores where
  runMessage msg t@(PoisonousSpores attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> PoisonousSpores <$> runMessage msg attrs
