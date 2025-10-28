module Arkham.Treachery.Cards.The13thVision (the13thVision) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype The13thVision = The13thVision TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

the13thVision :: TreacheryCard The13thVision
the13thVision = treachery The13thVision Cards.the13thVision

instance HasModifiersFor The13thVision where
  getModifiersFor (The13thVision a) = case a.placement of
    InThreatArea iid' ->
      getSkillTest >>= \case
        Nothing -> pure mempty
        Just st -> maybeModified_ a (SkillTestTarget st.id) do
          iid <- MaybeT getSkillTestInvestigator
          liftGuardM $ iid <=~> colocatedWith iid'
          pure [FailTies]
    _ -> pure ()

instance HasAbilities The13thVision where
  getAbilities (The13thVision a) = [restricted a 1 OnSameLocation doubleActionAbility]

instance RunMessage The13thVision where
  runMessage msg t@(The13thVision attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> The13thVision <$> liftRunMessage msg attrs
