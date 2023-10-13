module Arkham.Treachery.Helpers (
  module X,
  module Arkham.Treachery.Helpers,
) where

import Arkham.Game.Helpers as X

import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Target

attachTreachery :: (AsId a, IdOf a ~ TreacheryId, Targetable target) => a -> target -> Message
attachTreachery treachery target = AttachTreachery (asId treachery) (toTarget target)
