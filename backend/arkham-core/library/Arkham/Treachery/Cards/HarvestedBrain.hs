module Arkham.Treachery.Cards.HarvestedBrain
  ( harvestedBrain
  , HarvestedBrain(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype HarvestedBrain = HarvestedBrain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harvestedBrain :: TreacheryCard HarvestedBrain
harvestedBrain = treachery HarvestedBrain Cards.harvestedBrain

instance RunMessage HarvestedBrain where
  runMessage msg t@(HarvestedBrain attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> HarvestedBrain <$> runMessage msg attrs
