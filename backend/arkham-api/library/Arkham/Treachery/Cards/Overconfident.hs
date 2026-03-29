module Arkham.Treachery.Cards.Overconfident (overconfident) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Overconfident = Overconfident TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overconfident :: TreacheryCard Overconfident
overconfident = treachery Overconfident Cards.overconfident

instance HasAbilities Overconfident where
  getAbilities (Overconfident a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ SkillTestResult #after You #any #failure
    , restricted a 2 (InThreatAreaOf You) doubleActionAbility
    ]

instance RunMessage Overconfident where
  runMessage msg t@(Overconfident attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Overconfident <$> liftRunMessage msg attrs
