module Arkham.Treachery.Cards.TerrorInTheNight
  ( terrorInTheNight
  , TerrorInTheNight(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype TerrorInTheNight = TerrorInTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorInTheNight :: TreacheryCard TerrorInTheNight
terrorInTheNight = treachery TerrorInTheNight Cards.terrorInTheNight

instance RunMessage TerrorInTheNight where
  runMessage msg t@(TerrorInTheNight attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> TerrorInTheNight <$> runMessage msg attrs
