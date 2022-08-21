module Arkham.Treachery.Cards.FinalMistake
  ( finalMistake
  , FinalMistake(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype FinalMistake = FinalMistake TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalMistake :: TreacheryCard FinalMistake
finalMistake = treachery FinalMistake Cards.finalMistake

instance RunMessage FinalMistake where
  runMessage msg t@(FinalMistake attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> FinalMistake <$> runMessage msg attrs
