module Arkham.Treachery.Cards.The13thVision (the13thVision, The13thVision (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

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
    _ -> pure mempty

instance HasAbilities The13thVision where
  getAbilities (The13thVision a) =
    [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage The13thVision where
  runMessage msg t@(The13thVision attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> The13thVision <$> runMessage msg attrs
