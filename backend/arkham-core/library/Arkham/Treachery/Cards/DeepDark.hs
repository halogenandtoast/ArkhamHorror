module Arkham.Treachery.Cards.DeepDark
  ( deepDark
  , DeepDark(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype DeepDark = DeepDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepDark :: TreacheryCard DeepDark
deepDark = treachery DeepDark Cards.deepDark

instance RunMessage DeepDark where
  runMessage msg t@(DeepDark attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> DeepDark <$> runMessage msg attrs
