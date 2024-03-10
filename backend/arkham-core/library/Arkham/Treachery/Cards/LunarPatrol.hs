module Arkham.Treachery.Cards.LunarPatrol
  ( lunarPatrol
  , LunarPatrol(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LunarPatrol = LunarPatrol TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lunarPatrol :: TreacheryCard LunarPatrol
lunarPatrol = treachery LunarPatrol Cards.lunarPatrol

instance RunMessage LunarPatrol where
  runMessage msg t@(LunarPatrol attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> LunarPatrol <$> runMessage msg attrs
