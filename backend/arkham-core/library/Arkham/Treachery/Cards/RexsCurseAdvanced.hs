module Arkham.Treachery.Cards.RexsCurseAdvanced (RexsCurseAdvanced (..), rexsCurseAdvanced) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Metadata = Metadata {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RexsCurseAdvanced = RexsCurseAdvanced (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rexsCurseAdvanced :: TreacheryCard RexsCurseAdvanced
rexsCurseAdvanced = treachery (RexsCurseAdvanced . (`with` Metadata False)) Cards.rexsCurseAdvanced

instance HasAbilities RexsCurseAdvanced where
  getAbilities (RexsCurseAdvanced (x `With` _)) =
    [ playerLimit PerTestOrAbility
        $ restrictedAbility x 1 (InThreatAreaOf You)
        $ forced
        $ WouldHaveSkillTestResult #when You AnySkillTest #success
    ]

instance RunMessage RexsCurseAdvanced where
  runMessage msg t@(RexsCurseAdvanced (attrs `With` meta)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [DrawAnotherChaosToken iid, RerunSkillTest]
      pure $ RexsCurseAdvanced (attrs `with` Metadata True)
    FailedSkillTest iid _ _ _ _ _ | treacheryInThreatArea iid attrs -> do
      when (active meta) (shuffleIntoDeck iid attrs)
      pure $ RexsCurseAdvanced (attrs `With` Metadata False)
    SkillTestEnds {} -> pure $ RexsCurseAdvanced (attrs `With` Metadata False)
    _ -> RexsCurseAdvanced . (`with` meta) <$> liftRunMessage msg attrs
