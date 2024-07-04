module Arkham.Treachery.Cards.WhispersInYourHeadAnxiety (
  whispersInYourHeadAnxiety,
  WhispersInYourHeadAnxiety (..),
) where

import Arkham.Ability
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Import.Lifted

newtype WhispersInYourHeadAnxiety = WhispersInYourHeadAnxiety TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInYourHeadAnxiety :: TreacheryCard WhispersInYourHeadAnxiety
whispersInYourHeadAnxiety =
  treachery WhispersInYourHeadAnxiety Cards.whispersInYourHeadAnxiety

instance HasModifiersFor WhispersInYourHeadAnxiety where
  getModifiersFor (InvestigatorTarget iid) (WhispersInYourHeadAnxiety a) =
    modified a [CannotTriggerFastAbilities | treacheryInHandOf a == Just iid]
  getModifiersFor _ _ = pure []

instance HasAbilities WhispersInYourHeadAnxiety where
  getAbilities (WhispersInYourHeadAnxiety a) =
    [restrictedAbility a 1 InYourHand $ ActionAbility [] $ ActionCost 2]

instance RunMessage WhispersInYourHeadAnxiety where
  runMessage msg t@(WhispersInYourHeadAnxiety attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WhispersInYourHeadAnxiety <$> liftRunMessage msg attrs
