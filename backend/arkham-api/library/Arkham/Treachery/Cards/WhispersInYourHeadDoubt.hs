module Arkham.Treachery.Cards.WhispersInYourHeadDoubt (whispersInYourHeadDoubt) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher hiding (treacheryInHandOf)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhispersInYourHeadDoubt = WhispersInYourHeadDoubt TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadDoubt :: TreacheryCard WhispersInYourHeadDoubt
whispersInYourHeadDoubt = treachery WhispersInYourHeadDoubt Cards.whispersInYourHeadDoubt

instance HasModifiersFor WhispersInYourHeadDoubt where
  getModifiersFor (WhispersInYourHeadDoubt a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotPlay (CardWithType EventType)]
    _ -> pure ()

instance HasAbilities WhispersInYourHeadDoubt where
  getAbilities (WhispersInYourHeadDoubt a) = [restricted a 1 InYourHand doubleActionAbility]

instance RunMessage WhispersInYourHeadDoubt where
  runMessage msg t@(WhispersInYourHeadDoubt attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WhispersInYourHeadDoubt <$> liftRunMessage msg attrs
