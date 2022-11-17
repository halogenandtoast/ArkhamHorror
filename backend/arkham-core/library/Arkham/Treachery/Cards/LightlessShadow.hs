module Arkham.Treachery.Cards.LightlessShadow
  ( lightlessShadow
  , LightlessShadow(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LightlessShadow = LightlessShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightlessShadow :: TreacheryCard LightlessShadow
lightlessShadow = treachery LightlessShadow Cards.lightlessShadow

instance RunMessage LightlessShadow where
  runMessage msg t@(LightlessShadow attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> LightlessShadow <$> runMessage msg attrs
