module Arkham.Treachery.Cards.Pursued (pursued) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Pursued = Pursued TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pursued :: TreacheryCard Pursued
pursued = treachery Pursued Cards.pursued

instance HasAbilities Pursued where
  getAbilities (Pursued a) =
    [ restricted a 1 InYourThreatArea $ forced $ EnemyEnters #after YourLocation AnyEnemy
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage Pursued where
  runMessage msg t@(Pursued attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Pursued <$> liftRunMessage msg attrs
