module Arkham.Treachery.Cards.AbandonedAndAlone where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards (abandonedAndAlone)
import Arkham.Treachery.Runner

newtype AbandonedAndAlone = AbandonedAndAlone TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedAndAlone :: TreacheryCard AbandonedAndAlone
abandonedAndAlone = treachery AbandonedAndAlone Cards.abandonedAndAlone

instance RunMessage AbandonedAndAlone where
  runMessage msg t@(AbandonedAndAlone attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      pushAll [directHorror iid attrs 2, RemoveDiscardFromGame iid]
      pure t
    _ -> AbandonedAndAlone <$> runMessage msg attrs
