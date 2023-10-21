module Arkham.Treachery.Cards.BloodOnYourHands
  ( bloodOnYourHands
  , BloodOnYourHands(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BloodOnYourHands = BloodOnYourHands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodOnYourHands :: TreacheryCard BloodOnYourHands
bloodOnYourHands = treachery BloodOnYourHands Cards.bloodOnYourHands

instance RunMessage BloodOnYourHands where
  runMessage msg t@(BloodOnYourHands attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BloodOnYourHands <$> runMessage msg attrs
