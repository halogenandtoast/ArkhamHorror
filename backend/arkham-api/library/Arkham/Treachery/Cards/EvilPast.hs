module Arkham.Treachery.Cards.EvilPast (evilPast, EvilPast (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EvilPast = EvilPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evilPast :: TreacheryCard EvilPast
evilPast = treachery EvilPast Cards.evilPast

instance HasAbilities EvilPast where
  getAbilities (EvilPast a) =
    [skillTestAbility $ restrictedAbility a 1 (InThreatAreaOf You) $ forced EncounterDeckRunsOutOfCards]

instance RunMessage EvilPast where
  runMessage msg t@(EvilPast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasEvilPast <- selectAny $ treacheryIs Cards.evilPast <> Arkham.Matcher.treacheryInThreatAreaOf iid
      if hasEvilPast
        then gainSurge attrs
        else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 2
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> EvilPast <$> liftRunMessage msg attrs
