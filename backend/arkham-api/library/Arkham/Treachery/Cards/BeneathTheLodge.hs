module Arkham.Treachery.Cards.BeneathTheLodge (beneathTheLodge) where

import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestSource)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BeneathTheLodge = BeneathTheLodge TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beneathTheLodge :: TreacheryCard BeneathTheLodge
beneathTheLodge = treachery BeneathTheLodge Cards.beneathTheLodge

instance HasModifiersFor BeneathTheLodge where
  getModifiersFor (BeneathTheLodge attrs) = do
    hasKey <- fieldMap InvestigatorKeys notNull attrs.drawnBy
    modifySelf attrs [AddKeyword Keyword.Peril | hasKey]
    whenJustM getSkillTest \st -> maybeModified_ attrs (SkillTestTarget st.id) do
      source <- MaybeT getSkillTestSource
      guard $ isSource attrs source
      pure [Difficulty 1 | hasKey]

instance RunMessage BeneathTheLodge where
  runMessage msg t@(BeneathTheLodge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ HandlePointOfFailure iid (toTarget attrs) n
      pure t
    HandlePointOfFailure _ target 0 | isTarget attrs target -> pure t
    HandlePointOfFailure iid target n | isTarget attrs target -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      if hasClues
        then do
          chooseOneM iid $ withI18n do
            countVar 1 $ labeled' "loseClues" $ removeClues attrs iid 1
            countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
          push $ HandlePointOfFailure iid (toTarget attrs) (n - 1)
        else do
          assignHorror iid attrs 1
          push $ HandlePointOfFailure iid (toTarget attrs) (n - 1)
      pure t
    _ -> BeneathTheLodge <$> liftRunMessage msg attrs
