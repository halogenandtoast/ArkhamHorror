module Arkham.Homebrew.AgainstTheWendigo.Treacheries.OldInjury (oldInjury) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype OldInjury = OldInjury TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldInjury :: TreacheryCard OldInjury
oldInjury = treachery OldInjury Cards.oldInjury

instance HasAbilities OldInjury where
  getAbilities (OldInjury a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    , restricted a 2 (InThreatAreaOf You) doubleActionAbility
    ]

instance RunMessage OldInjury where
  runMessage msg t@(OldInjury attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> OldInjury <$> liftRunMessage msg attrs
