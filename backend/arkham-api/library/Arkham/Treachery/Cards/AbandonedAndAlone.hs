module Arkham.Treachery.Cards.AbandonedAndAlone (abandonedAndAlone) where

import Arkham.Script
import Arkham.Treachery.Cards qualified as Cards (abandonedAndAlone)
import Arkham.Treachery.Import.Lifted hiding (directHorror)

newtype AbandonedAndAlone = AbandonedAndAlone TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable)

abandonedAndAlone :: TreacheryCard AbandonedAndAlone
abandonedAndAlone = treachery AbandonedAndAlone Cards.abandonedAndAlone

instance RunMessage AbandonedAndAlone where
  runMessage = script $ revelation do
    directHorror you 2
    removeDiscardFromGame
