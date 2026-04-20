module Arkham.Treachery.Cards.BlasphemousInvocation (blasphemousInvocation) where

import Arkham.GameValue (GameValue (..))
import Arkham.Matcher
import Arkham.Message.Lifted hiding (beginSkillTest)
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
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      cultistsWithNoDoom <- select $ EnemyWithTrait Cultist <> EnemyWithDoom (EqualTo $ Static 0)
      placed <- if notNull cultistsWithNoDoom
        then do
          targets <- take n <$> shuffleM cultistsWithNoDoom
          for_ targets $ \target -> placeDoom (toSource attrs) target 1
          pure $ length targets
        else pure 0
      when (placed == 0) $ 
        findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Cultist
      pure t
    _ -> BlasphemousInvocation <$> liftRunMessage msg attrs
