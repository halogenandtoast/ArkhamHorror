module Arkham.Treachery.Cards.WhatHaveYouDone
  ( whatHaveYouDone
  , WhatHaveYouDone(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WhatHaveYouDone = WhatHaveYouDone TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: TreacheryCard WhatHaveYouDone
whatHaveYouDone = treachery WhatHaveYouDone Cards.whatHaveYouDone

instance RunMessage WhatHaveYouDone where
  runMessage msg t@(WhatHaveYouDone attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> WhatHaveYouDone <$> runMessage msg attrs
