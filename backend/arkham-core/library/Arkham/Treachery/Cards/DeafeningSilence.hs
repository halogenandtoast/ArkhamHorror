module Arkham.Treachery.Cards.DeafeningSilence (deafeningSilence, DeafeningSilence (..)) where

import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeafeningSilence = DeafeningSilence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deafeningSilence :: TreacheryCard DeafeningSilence
deafeningSilence = treachery DeafeningSilence Cards.deafeningSilence

instance RunMessage DeafeningSilence where
  runMessage msg t@(DeafeningSilence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> AssetWithAnyDoom
      if notNull assets
        then chooseOne iid $ targetLabels assets $ only . handleTargetChoice iid attrs
        else shuffleIntoDeck iid attrs
      pure t
    HandleTargetChoice _iid (isSource attrs -> True) (AssetTarget aid) -> do
      removeDoom attrs aid 1
      placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> DeafeningSilence <$> liftRunMessage msg attrs
