module Arkham.Treachery.Cards.TorturedVisions (torturedVisions) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator, isSkillTestSource)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TorturedVisions = TorturedVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturedVisions :: TreacheryCard TorturedVisions
torturedVisions = treachery TorturedVisions Cards.torturedVisions

instance HasModifiersFor TorturedVisions where
  getModifiersFor (TorturedVisions attrs) = do
    getSkillTest >>= \case
      Nothing -> pure ()
      Just st -> maybeModified_ attrs (SkillTestTarget st.id) do
        liftGuardM $ isSkillTestSource attrs
        iid <- MaybeT getSkillTestInvestigator
        handSize <- lift $ fieldMap InvestigatorHand length iid
        guard $ handSize <= 3
        pure [Difficulty 2]

instance RunMessage TorturedVisions where
  runMessage msg t@(TorturedVisions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directHorror iid attrs 1
      randomDiscard iid attrs
      pure t
    _ -> TorturedVisions <$> liftRunMessage msg attrs
