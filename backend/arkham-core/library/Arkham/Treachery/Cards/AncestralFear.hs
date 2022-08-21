module Arkham.Treachery.Cards.AncestralFear
  ( ancestralFear
  , AncestralFear(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype AncestralFear = AncestralFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancestralFear :: TreacheryCard AncestralFear
ancestralFear = treachery AncestralFear Cards.ancestralFear

instance RunMessage AncestralFear where
  runMessage msg t@(AncestralFear attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> AncestralFear <$> runMessage msg attrs
