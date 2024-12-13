module Arkham.Treachery.Cards.Totality (totality, Totality (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Totality = Totality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

totality :: TreacheryCard Totality
totality = treachery Totality Cards.totality

instance HasAbilities Totality where
  getAbilities (Totality a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ Enters #after You FloodedLocation
    , restricted a 2 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    ]

instance RunMessage Totality where
  runMessage msg t@(Totality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Totality <$> liftRunMessage msg attrs
