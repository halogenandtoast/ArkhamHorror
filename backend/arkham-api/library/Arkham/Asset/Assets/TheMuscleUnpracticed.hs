module Arkham.Asset.Assets.TheMuscleUnpracticed (theMuscleUnpracticed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Matcher hiding (InvestigatorEliminated)
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Trait (Trait (Casino))

newtype TheMuscleUnpracticed = TheMuscleUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMuscleUnpracticed :: AssetCard TheMuscleUnpracticed
theMuscleUnpracticed = asset TheMuscleUnpracticed Cards.theMuscleUnpracticed

instance HasAbilities TheMuscleUnpracticed where
  getAbilities (TheMuscleUnpracticed a) =
    [controlled_ a 1 $ triggered (EnemyDefeated #after You ByAny (EnemyWithTrait Casino)) (exhaust a)]

instance RunMessage TheMuscleUnpracticed where
  runMessage msg a@(TheMuscleUnpracticed attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      byTwo <-
        selectNone $ EnemyAt (orConnected_ (locationWithInvestigator iid)) <> not_ (EnemyWithId eid)
      reduceAlarmLevelBy (if byTwo then 2 else 1) (attrs.ability 1) iid
      pure a
    InvestigatorEliminated _ -> pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theMusclePracticed
      pure a
    _ -> TheMuscleUnpracticed <$> liftRunMessage msg attrs
