module Arkham.Treachery.Cards.PutridVapors (putridVapors) where

import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype PutridVapors = PutridVapors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

putridVapors :: TreacheryCard PutridVapors
putridVapors = treachery PutridVapors Cards.putridVapors

instance RunMessage PutridVapors where
  runMessage msg t@(PutridVapors attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid (toSource attrs) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      hasAssets <- selectAny $ AssetControlledBy (InvestigatorWithId iid) <> not_ StoryAsset
      when hasAssets do
        chooseAndDiscardAssetMatching iid attrs $ AssetControlledBy (InvestigatorWithId iid) <> not_ StoryAsset
      pure t
    _ -> PutridVapors <$> liftRunMessage msg attrs
