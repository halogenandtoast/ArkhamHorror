module Arkham.Treachery.Cards.CalledByTheMists (calledByTheMists, CalledByTheMists (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CalledByTheMists = CalledByTheMists TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

calledByTheMists :: TreacheryCard CalledByTheMists
calledByTheMists = treachery CalledByTheMists Cards.calledByTheMists

instance HasAbilities CalledByTheMists where
  getAbilities (CalledByTheMists a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ InitiatedSkillTest #after You AnySkillType (SkillTestGameValue $ atLeast 4) #any
    , restrictedAbility a 2 OnSameLocation $ ActionAbility [] $ ActionCost 2
    ]

instance RunMessage CalledByTheMists where
  runMessage msg t@(CalledByTheMists attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> CalledByTheMists <$> liftRunMessage msg attrs
