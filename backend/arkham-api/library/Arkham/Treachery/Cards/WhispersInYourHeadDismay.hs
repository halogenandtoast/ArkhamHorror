module Arkham.Treachery.Cards.WhispersInYourHeadDismay (whispersInYourHeadDismay) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Helpers.Modifiers (ModifierType (CannotCommitCards), modified_)
import Arkham.Matcher hiding (treacheryInHandOf)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhispersInYourHeadDismay = WhispersInYourHeadDismay TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDismay :: TreacheryCard WhispersInYourHeadDismay
whispersInYourHeadDismay = treachery WhispersInYourHeadDismay Cards.whispersInYourHeadDismay

instance HasModifiersFor WhispersInYourHeadDismay where
  getModifiersFor (WhispersInYourHeadDismay a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotCommitCards $ CardWithType SkillType]
    _ -> pure ()

instance HasAbilities WhispersInYourHeadDismay where
  getAbilities (WhispersInYourHeadDismay a) = [restricted a 1 InYourHand doubleActionAbility]

instance RunMessage WhispersInYourHeadDismay where
  runMessage msg t@(WhispersInYourHeadDismay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WhispersInYourHeadDismay <$> liftRunMessage msg attrs
