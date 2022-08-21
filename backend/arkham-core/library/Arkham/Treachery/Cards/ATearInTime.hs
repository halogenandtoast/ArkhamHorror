module Arkham.Treachery.Cards.ATearInTime
  ( aTearInTime
  , ATearInTime(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype ATearInTime = ATearInTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInTime :: TreacheryCard ATearInTime
aTearInTime = treachery ATearInTime Cards.aTearInTime

instance RunMessage ATearInTime where
  runMessage msg t@(ATearInTime attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> ATearInTime <$> runMessage msg attrs
