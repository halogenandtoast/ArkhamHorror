module Arkham.Treachery.Cards.GlowingEyes
  ( glowingEyes
  , GlowingEyes(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype GlowingEyes = GlowingEyes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glowingEyes :: TreacheryCard GlowingEyes
glowingEyes = treachery GlowingEyes Cards.glowingEyes

instance RunMessage GlowingEyes where
  runMessage msg t@(GlowingEyes attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GlowingEyes <$> runMessage msg attrs
