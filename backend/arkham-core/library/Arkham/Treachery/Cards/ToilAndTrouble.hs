module Arkham.Treachery.Cards.ToilAndTrouble
  ( toilAndTrouble
  , ToilAndTrouble(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ToilAndTrouble = ToilAndTrouble TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toilAndTrouble :: TreacheryCard ToilAndTrouble
toilAndTrouble = treachery ToilAndTrouble Cards.toilAndTrouble

instance RunMessage ToilAndTrouble where
  runMessage msg t@(ToilAndTrouble attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ToilAndTrouble <$> runMessage msg attrs
