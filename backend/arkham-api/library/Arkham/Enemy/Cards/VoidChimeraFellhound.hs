module Arkham.Enemy.Cards.VoidChimeraFellhound (voidChimeraFellhound) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isEvading, isFighting)
import Arkham.Keyword qualified as Keyword

newtype VoidChimeraFellhound = VoidChimeraFellhound EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraFellhound :: EnemyCard VoidChimeraFellhound
voidChimeraFellhound = enemy VoidChimeraFellhound Cards.voidChimeraFellhound (4, Static 2, 4) (2, 1)

instance HasModifiersFor VoidChimeraFellhound where
  getModifiersFor (VoidChimeraFellhound a) = do
    runMaybeT_ do
      iid <- MaybeT getSkillTestInvestigator
      liftGuardM $ liftA2 (||) (isEvading a) (isFighting a)
      mods <- lift $ getModifiers iid
      guard $ notNull [() | Hollow _ <- mods]
      modifySelf a [AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]
