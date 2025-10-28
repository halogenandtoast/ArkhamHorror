module Arkham.Treachery.Cards.TrappedSpirits (trappedSpirits) where

import Arkham.Cost
import Arkham.Helpers.Modifiers (ModifierType (CommitCost), modifySelectMaybe)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestSource)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TrappedSpirits = TrappedSpirits TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappedSpirits :: TreacheryCard TrappedSpirits
trappedSpirits = treachery TrappedSpirits Cards.trappedSpirits

instance HasModifiersFor TrappedSpirits where
  getModifiersFor (TrappedSpirits a) = modifySelectMaybe a Anyone \_ -> do
    source <- MaybeT getSkillTestSource
    iid <- MaybeT getSkillTestInvestigator
    guard $ isSource a source
    lid <- MaybeT $ field InvestigatorLocation iid
    liftGuardM $ lid <=~> HauntedLocation
    pure [CommitCost (ResolveEachHauntedAbility lid)]

instance RunMessage TrappedSpirits where
  runMessage msg t@(TrappedSpirits attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs n
      pure t
    _ -> TrappedSpirits <$> liftRunMessage msg attrs
