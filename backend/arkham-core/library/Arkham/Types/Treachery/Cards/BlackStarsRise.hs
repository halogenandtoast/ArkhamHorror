module Arkham.Types.Treachery.Cards.BlackStarsRise
  ( blackStarsRise
  , BlackStarsRise(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BlackStarsRise = BlackStarsRise TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: TreacheryCard BlackStarsRise
blackStarsRise = treachery BlackStarsRise Cards.blackStarsRise

instance TreacheryRunner env => RunMessage env BlackStarsRise where
  runMessage msg t@(BlackStarsRise attrs) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> BlackStarsRise <$> runMessage msg attrs
