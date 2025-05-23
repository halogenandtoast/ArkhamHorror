module Arkham.Treachery.Cards.WhispersInYourHeadDread (whispersInYourHeadDread) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhispersInYourHeadDread = WhispersInYourHeadDread TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDread :: TreacheryCard WhispersInYourHeadDread
whispersInYourHeadDread = treachery WhispersInYourHeadDread Cards.whispersInYourHeadDread

instance HasModifiersFor WhispersInYourHeadDread where
  getModifiersFor (WhispersInYourHeadDread a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotMoveMoreThanOnceEachTurn]
    _ -> pure ()

instance HasAbilities WhispersInYourHeadDread where
  getAbilities (WhispersInYourHeadDread a) = [restricted a 1 InYourHand doubleActionAbility]

instance RunMessage WhispersInYourHeadDread where
  runMessage msg t@(WhispersInYourHeadDread attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WhispersInYourHeadDread <$> liftRunMessage msg attrs
