module Arkham.Treachery.Cards.BlasphemousInvocation (blasphemousInvocation) where

import Arkham.Matcher
import Arkham.Message.Lifted hiding (beginSkillTest)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BlasphemousInvocation = BlasphemousInvocation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blasphemousInvocation :: TreacheryCard BlasphemousInvocation
blasphemousInvocation = treachery BlasphemousInvocation Cards.blasphemousInvocation

instance RunMessage BlasphemousInvocation where
  runMessage msg t@(BlasphemousInvocation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      cultists <- selectCount $ #cultist <> EnemyWithoutDoom <> CanPlaceDoomOnEnemy
      if cultists > 0
        then doStep n msg
        else findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Cultist
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      cultists <- select $ #cultist <> EnemyWithoutDoom <> CanPlaceDoomOnEnemy
      chooseTargetM iid cultists \cultist -> do
        placeDoom attrs cultist 1
        doStep (n - 1) msg'
      pure t
    _ -> BlasphemousInvocation <$> liftRunMessage msg attrs
