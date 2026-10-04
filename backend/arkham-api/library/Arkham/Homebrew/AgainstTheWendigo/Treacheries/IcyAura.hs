module Arkham.Homebrew.AgainstTheWendigo.Treacheries.IcyAura (icyAura) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype IcyAura = IcyAura TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icyAura :: TreacheryCard IcyAura
icyAura = treachery IcyAura Cards.icyAura

instance HasModifiersFor IcyAura where
  -- "You cannot play events or commit cards to a skill test."
  getModifiersFor (IcyAura attrs) =
    inThreatAreaGets attrs [CannotPlay #event, CannotCommitCards AnyCard]

instance HasAbilities IcyAura where
  getAbilities (IcyAura a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage IcyAura where
  runMessage msg t@(IcyAura attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> IcyAura <$> liftRunMessage msg attrs
