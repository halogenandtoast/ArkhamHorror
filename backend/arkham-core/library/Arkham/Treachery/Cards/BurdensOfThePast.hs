module Arkham.Treachery.Cards.BurdensOfThePast
  ( burdensOfThePast
  , BurdensOfThePast(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BurdensOfThePast = BurdensOfThePast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burdensOfThePast :: TreacheryCard BurdensOfThePast
burdensOfThePast = treachery BurdensOfThePast Cards.burdensOfThePast

instance RunMessage BurdensOfThePast where
  runMessage msg t@(BurdensOfThePast attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BurdensOfThePast <$> runMessage msg attrs
