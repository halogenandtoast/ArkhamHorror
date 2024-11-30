module Arkham.Treachery.Cards.WillOfTheSpiderMother (willOfTheSpiderMother, WillOfTheSpiderMother (..)) where

import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestSource)
import Arkham.Matcher
import Arkham.Trait (Trait (Spider))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WillOfTheSpiderMother = WillOfTheSpiderMother TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willOfTheSpiderMother :: TreacheryCard WillOfTheSpiderMother
willOfTheSpiderMother = treachery WillOfTheSpiderMother Cards.willOfTheSpiderMother

instance HasModifiersFor WillOfTheSpiderMother where
  getModifiersFor (InvestigatorTarget iid) (WillOfTheSpiderMother attrs) = maybeModified attrs do
    iid' <- MaybeT getSkillTestInvestigator
    guard $ iid == iid'
    source <- MaybeT getSkillTestSource
    guard $ isSource attrs source
    liftGuardM $ iid <=~> InvestigatorAt (LocationWithEnemy $ withTrait Spider)
    pure [CannotCommitCards AnyCard]
  getModifiersFor _ _ = pure []

instance RunMessage WillOfTheSpiderMother where
  runMessage msg t@(WillOfTheSpiderMother attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- genId
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      roundModifiers attrs iid [CannotFight AnyEnemy, CannotInvestigate]
      pure t
    _ -> WillOfTheSpiderMother <$> liftRunMessage msg attrs
