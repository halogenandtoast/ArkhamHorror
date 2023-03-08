module Arkham.Treachery.Cards.RealmOfTorment
  ( realmOfTorment
  , RealmOfTorment(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RealmOfTorment = RealmOfTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realmOfTorment :: TreacheryCard RealmOfTorment
realmOfTorment = treachery RealmOfTorment Cards.realmOfTorment

instance HasAbilities RealmOfTorment where
  getAbilities (RealmOfTorment a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ TurnBegins
      Timing.When
      You
    , restrictedAbility a 2 (InThreatAreaOf You) $ ForcedAbility $ TurnEnds
      Timing.When
      You
    ]

instance RunMessage RealmOfTorment where
  runMessage msg t@(RealmOfTorment attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (InvestigatorTarget iid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      runHauntedAbilities iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ RevelationSkillTest iid (toSource attrs) SkillWillpower 3
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Discard (toAbilitySource attrs 1) $ toTarget attrs
        pure t
    _ -> RealmOfTorment <$> runMessage msg attrs
