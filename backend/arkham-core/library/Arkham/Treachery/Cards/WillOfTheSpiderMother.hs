module Arkham.Treachery.Cards.WillOfTheSpiderMother (willOfTheSpiderMother, WillOfTheSpiderMother (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Trait (Trait (Spider))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WillOfTheSpiderMother = WillOfTheSpiderMother TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

willOfTheSpiderMother :: TreacheryCard WillOfTheSpiderMother
willOfTheSpiderMother = treachery WillOfTheSpiderMother Cards.willOfTheSpiderMother

instance HasModifiersFor WillOfTheSpiderMother where
  getModifiersFor (InvestigatorTarget iid) (WillOfTheSpiderMother attrs) = do
    mSkillTestInvestigator <- getSkillTestInvestigator
    if Just iid == mSkillTestInvestigator
      then do
        mSource <- getSkillTestSource
        hasSpider <- iid <=~> InvestigatorAt (LocationWithEnemy $ EnemyWithTrait Spider)
        pure
          $ toModifiers
            attrs
            [CannotCommitCards AnyCard | hasSpider, source <- maybeToList mSource, attrs `isSource` source]
      else pure []
  getModifiersFor _ _ = pure []

instance RunMessage WillOfTheSpiderMother where
  runMessage msg t@(WillOfTheSpiderMother attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower 3
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ roundModifiers attrs iid [CannotFight AnyEnemy, CannotInvestigate]
      pure t
    _ -> WillOfTheSpiderMother <$> runMessage msg attrs
