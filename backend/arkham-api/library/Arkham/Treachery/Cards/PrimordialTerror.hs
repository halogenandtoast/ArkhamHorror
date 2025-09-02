module Arkham.Treachery.Cards.PrimordialTerror (primordialTerror) where

import Arkham.Card
import Arkham.Helpers.Enemy
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Dinosaur))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PrimordialTerror = PrimordialTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primordialTerror :: TreacheryCard PrimordialTerror
primordialTerror = treachery PrimordialTerror Cards.primordialTerror

instance RunMessage PrimordialTerror where
  runMessage msg t@(PrimordialTerror attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      when (n > 0) $ assignHorror iid attrs n
      when (n >= 3) $ findEncounterCard iid attrs $ card_ $ #enemy <> withTrait Dinosaur
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyWith_ card Unplaced $ createEngagedWith iid
      pure t
    _ -> PrimordialTerror <$> liftRunMessage msg attrs
