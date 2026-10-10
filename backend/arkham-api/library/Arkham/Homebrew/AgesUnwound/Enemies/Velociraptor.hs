module Arkham.Homebrew.AgesUnwound.Enemies.Velociraptor (velociraptor) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Velociraptor = Velociraptor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Alert, Hunter and Retaliate are all on the card def.
velociraptor :: EnemyCard Velociraptor
velociraptor = enemy Velociraptor Cards.velociraptor

{- | "Forced - After Velociraptor engages an investigator during the mythos or
investigator phases: It makes an immediate attack."

"an investigator", not "you": the engagement names whoever it caught, so the
window's @Who@ is @Anyone@ and the attack follows the engagement.
-}
instance HasAbilities Velociraptor where
  getAbilities (Velociraptor a) =
    extend1 a
      $ restricted a 1 (oneOf [DuringPhase #mythos, DuringPhase #investigation])
      $ forced
      $ EnemyEngaged #after Anyone (be a)

instance RunMessage Velociraptor where
  runMessage msg e@(Velociraptor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> Velociraptor <$> liftRunMessage msg attrs
