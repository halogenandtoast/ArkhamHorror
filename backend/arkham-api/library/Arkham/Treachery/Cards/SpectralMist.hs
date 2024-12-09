module Arkham.Treachery.Cards.SpectralMist (SpectralMist (..), spectralMist) where

import Arkham.Ability
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Source
import Arkham.Trait (Trait (Bayou))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype SpectralMist = SpectralMist TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralMist :: TreacheryCard SpectralMist
spectralMist = treachery SpectralMist Cards.spectralMist

instance HasModifiersFor SpectralMist where
  getModifiersFor (SpectralMist a) = case a.placement of
    AttachedToLocation lid ->
      getSkillTest >>= \case
        Nothing -> pure mempty
        Just st -> maybeModified_ a (SkillTestTarget st.id) do
          iid <- MaybeT getSkillTestInvestigator
          lid' <- MaybeT $ getMaybeLocation iid
          guard $ lid == lid'
          pure [Difficulty 1]
    _ -> pure mempty

instance HasAbilities SpectralMist where
  getAbilities (SpectralMist a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage SpectralMist where
  runMessage msg t@(SpectralMist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      targets <-
        selectTargets $ withTrait Bayou <> not_ (LocationWithTreachery $ treacheryIs Cards.spectralMist)
      when (notNull targets) $ chooseOne iid $ targetLabels targets $ only . Msg.attachTreachery attrs
      SpectralMist <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SpectralMist <$> liftRunMessage msg attrs
