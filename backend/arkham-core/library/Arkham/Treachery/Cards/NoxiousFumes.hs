module Arkham.Treachery.Cards.NoxiousFumes
  ( noxiousFumes
  , NoxiousFumes(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NoxiousFumes = NoxiousFumes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noxiousFumes :: TreacheryCard NoxiousFumes
noxiousFumes = treachery NoxiousFumes Cards.noxiousFumes

instance RunMessage NoxiousFumes where
  runMessage msg t@(NoxiousFumes attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> NoxiousFumes <$> runMessage msg attrs
