module Arkham.Treachery.Cards.OtherworldlyVisions (otherworldlyVisions) where

import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator, isSkillTestSource)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Projection
import Arkham.Trait (Trait (Colour))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OtherworldlyVisions = OtherworldlyVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyVisions :: TreacheryCard OtherworldlyVisions
otherworldlyVisions = treachery OtherworldlyVisions Cards.otherworldlyVisions

instance HasModifiersFor OtherworldlyVisions where
  getModifiersFor (OtherworldlyVisions attrs) = do
    getSkillTest >>= \case
      Nothing -> pure ()
      Just st -> maybeModified_ attrs (SkillTestTarget st.id) do
        liftGuardM $ isSkillTestSource attrs
        iid <- MaybeT getSkillTestInvestigator
        liftGuardM $ selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Colour
        pure [Difficulty 2]

instance RunMessage OtherworldlyVisions where
  runMessage msg t@(OtherworldlyVisions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      hasCards <- fieldMap InvestigatorHand notNull iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" do
          assignHorror iid attrs 1
          doStep (n - 1) msg'
        countVar 1 $ labeledValidate' hasCards "discardRandomCardsFromHand" do
          randomDiscard iid attrs
          doStep (n - 1) msg'
      pure t
    _ -> OtherworldlyVisions <$> liftRunMessage msg attrs
