module Arkham.Treachery.Cards.WhispersInYourHeadAnxiety (whispersInYourHeadAnxiety) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhispersInYourHeadAnxiety = WhispersInYourHeadAnxiety TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadAnxiety :: TreacheryCard WhispersInYourHeadAnxiety
whispersInYourHeadAnxiety =
  treachery WhispersInYourHeadAnxiety Cards.whispersInYourHeadAnxiety

instance HasModifiersFor WhispersInYourHeadAnxiety where
  getModifiersFor (WhispersInYourHeadAnxiety a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotTriggerFastAbilities]
    _ -> pure ()

instance HasAbilities WhispersInYourHeadAnxiety where
  getAbilities (WhispersInYourHeadAnxiety a) = [restricted a 1 InYourHand doubleActionAbility]

instance RunMessage WhispersInYourHeadAnxiety where
  runMessage msg t@(WhispersInYourHeadAnxiety attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WhispersInYourHeadAnxiety <$> liftRunMessage msg attrs
