module Arkham.Homebrew.TheInvisibleMan.Acts.SeeThroughToTheEnd (seeThroughToTheEnd) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher

newtype SeeThroughToTheEnd = SeeThroughToTheEnd ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seeThroughToTheEnd :: ActCard SeeThroughToTheEnd
seeThroughToTheEnd = act (2, A) SeeThroughToTheEnd Cards.seeThroughToTheEnd Nothing

{- | "If there is 8{perInvestigator} or more damage on Griffin, advance."
Griffin has no health of his own, so this is the only thing counting the damage
on him.
-}
instance HasAbilities SeeThroughToTheEnd where
  getAbilities = actAbilities1 \x ->
    restricted
      x
      1
      (HasCalculation (SumEnemyFieldCalculation griffin EnemyDamage) (AtLeast $ PerPlayer 8))
      $ Objective
      $ forced AnyWindow

instance RunMessage SeeThroughToTheEnd where
  runMessage msg a@(SeeThroughToTheEnd attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> SeeThroughToTheEnd <$> liftRunMessage msg attrs
