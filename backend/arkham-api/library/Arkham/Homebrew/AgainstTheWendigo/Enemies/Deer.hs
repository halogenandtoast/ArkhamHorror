module Arkham.Homebrew.AgainstTheWendigo.Enemies.Deer (deer) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Firearm, Ranged))

newtype Deer = Deer EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deer :: EnemyCard Deer
deer = enemy Deer Cards.deer

instance HasModifiersFor Deer where
  {- | "You cannot place damage on the Deer, unless you perform a Fight action
  from a Firearm or a Ranged card." -}
  getModifiersFor (Deer a) =
    modifySelf
      a
      [CannotBeDamagedByPlayerSourcesExcept $ SourceMatchesAny [SourceWithTrait Firearm, SourceWithTrait Ranged]]

instance HasAbilities Deer where
  getAbilities (Deer a) =
    [restricted a 1 (exists $ be a <> EnemyIsEngagedWith Anyone) $ forced $ PhaseEnds #when #enemy]

instance RunMessage Deer where
  runMessage msg e@(Deer attrs) = runQueueT $ case msg of
    -- "Forced - At the end of each enemy phase: Disengage the Deer."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      disengageFromAll attrs
      pure e
    _ -> Deer <$> liftRunMessage msg attrs
