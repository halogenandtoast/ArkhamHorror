module Arkham.Treachery.Cards.CallToOrder
  ( callToOrder
  , CallToOrder(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CallToOrder = CallToOrder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callToOrder :: TreacheryCard CallToOrder
callToOrder = treachery CallToOrder Cards.callToOrder

instance RunMessage CallToOrder where
  runMessage msg t@(CallToOrder attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CallToOrder <$> runMessage msg attrs
