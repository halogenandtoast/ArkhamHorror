module Arkham.Treachery.Cards.DeceptiveMemories
  ( deceptiveMemories
  , DeceptiveMemories(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DeceptiveMemories = DeceptiveMemories TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deceptiveMemories :: TreacheryCard DeceptiveMemories
deceptiveMemories = treachery DeceptiveMemories Cards.deceptiveMemories

instance RunMessage DeceptiveMemories where
  runMessage msg t@(DeceptiveMemories attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DeceptiveMemories <$> runMessage msg attrs
