module Arkham.Treachery.Cards.RealmOfTorment (realmOfTorment, RealmOfTorment (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RealmOfTorment = RealmOfTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realmOfTorment :: TreacheryCard RealmOfTorment
realmOfTorment = treachery RealmOfTorment Cards.realmOfTorment

instance HasAbilities RealmOfTorment where
  getAbilities (RealmOfTorment a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ forced $ TurnBegins #when You
    , restrictedAbility a 2 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    ]

instance RunMessage RealmOfTorment where
  runMessage msg t@(RealmOfTorment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      runHauntedAbilities iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      revelationSkillTest iid (attrs.ability 2) #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> RealmOfTorment <$> liftRunMessage msg attrs
