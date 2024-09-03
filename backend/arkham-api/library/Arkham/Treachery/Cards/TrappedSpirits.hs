module Arkham.Treachery.Cards.TrappedSpirits (trappedSpirits, TrappedSpirits (..)) where

import Arkham.Classes
import Arkham.Cost
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TrappedSpirits = TrappedSpirits TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappedSpirits :: TreacheryCard TrappedSpirits
trappedSpirits = treachery TrappedSpirits Cards.trappedSpirits

instance HasModifiersFor TrappedSpirits where
  getModifiersFor (InvestigatorTarget _) (TrappedSpirits a) = maybeModified a do
    source <- MaybeT getSkillTestSource
    iid <- MaybeT getSkillTestInvestigator
    guard $ isSource a source
    lid <- MaybeT $ field InvestigatorLocation iid
    liftGuardM $ lid <=~> HauntedLocation
    pure [CommitCost (ResolveEachHauntedAbility lid)]
  getModifiersFor _ _ = pure []

instance RunMessage TrappedSpirits where
  runMessage msg t@(TrappedSpirits attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ assignDamage iid attrs n
      pure t
    _ -> TrappedSpirits <$> runMessage msg attrs
