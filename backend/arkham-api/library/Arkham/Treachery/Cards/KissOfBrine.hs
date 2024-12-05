module Arkham.Treachery.Cards.KissOfBrine (kissOfBrine, KissOfBrine (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator, isSkillTestSource)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype KissOfBrine = KissOfBrine TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kissOfBrine :: TreacheryCard KissOfBrine
kissOfBrine = treachery KissOfBrine Cards.kissOfBrine

instance HasModifiersFor KissOfBrine where
  getModifiersFor (KissOfBrine attrs) = do
    threat <- inThreatAreaGets attrs [CannotGainResources, CannotDrawCards]
    skillTest <-
      getSkillTest >>= \case
        Nothing -> pure mempty
        Just st -> maybeModified_ attrs (SkillTestTarget st.id) do
          liftGuardM $ isSkillTestSource attrs
          iid <- MaybeT getSkillTestInvestigator
          liftGuardM $ iid <=~> InvestigatorAt FloodedLocation
          pure [Difficulty 2]
    pure $ threat <> skillTest

instance HasAbilities KissOfBrine where
  getAbilities (KissOfBrine a) = [mkAbility a 1 $ forced $ PhaseEnds #when #enemy]

instance RunMessage KissOfBrine where
  runMessage msg t@(KissOfBrine attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> KissOfBrine <$> liftRunMessage msg attrs
