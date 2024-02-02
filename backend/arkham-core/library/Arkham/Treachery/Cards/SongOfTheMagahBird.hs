module Arkham.Treachery.Cards.SongOfTheMagahBird ( songOfTheMagahBird , SongOfTheMagahBird(..)) where

import Arkham.Prelude
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SongOfTheMagahBird = SongOfTheMagahBird TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

songOfTheMagahBird :: TreacheryCard SongOfTheMagahBird
songOfTheMagahBird = treachery SongOfTheMagahBird Cards.songOfTheMagahBird

instance RunMessage SongOfTheMagahBird where
  runMessage msg t@(SongOfTheMagahBird attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SongOfTheMagahBird <$> runMessage msg attrs
