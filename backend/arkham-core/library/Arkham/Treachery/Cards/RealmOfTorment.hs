module Arkham.Treachery.Cards.RealmOfTorment (
  realmOfTorment,
  RealmOfTorment (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RealmOfTorment = RealmOfTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realmOfTorment :: TreacheryCard RealmOfTorment
realmOfTorment = treachery RealmOfTorment Cards.realmOfTorment

instance HasAbilities RealmOfTorment where
  getAbilities (RealmOfTorment a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnBegins #when You
    , restrictedAbility a 2 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds #when You
    ]

instance RunMessage RealmOfTorment where
  runMessage msg t@(RealmOfTorment attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      runHauntedAbilities iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ RevelationSkillTest iid (toAbilitySource attrs 2) #willpower 3
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> RealmOfTorment <$> runMessage msg attrs
