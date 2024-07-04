module Arkham.Treachery.Cards.SelfDestructive (selfDestructive, SelfDestructive (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SelfDestructive = SelfDestructive TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selfDestructive :: TreacheryCard SelfDestructive
selfDestructive = treachery SelfDestructive Cards.selfDestructive

instance HasAbilities SelfDestructive where
  getAbilities (SelfDestructive a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ EnemyDealtDamage #when AnyDamageEffect AnyEnemy
        $ SourceOwnedBy You
    , restrictedAbility a 2 OnSameLocation $ ActionAbility [] $ ActionCost 2
    ]

instance RunMessage SelfDestructive where
  runMessage msg t@(SelfDestructive attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SelfDestructive <$> liftRunMessage msg attrs
