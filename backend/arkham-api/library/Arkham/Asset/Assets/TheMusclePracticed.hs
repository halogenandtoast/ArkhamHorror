module Arkham.Asset.Assets.TheMusclePracticed (theMusclePracticed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Matcher hiding (InvestigatorEliminated)
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Trait (Trait (Casino))

newtype TheMusclePracticed = TheMusclePracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMusclePracticed :: AssetCard TheMusclePracticed
theMusclePracticed = asset TheMusclePracticed Cards.theMusclePracticed

instance HasAbilities TheMusclePracticed where
  getAbilities (TheMusclePracticed a) =
    [controlled_ a 1 $ triggered (EnemyDefeated #after You ByAny (EnemyWithTrait Casino)) (exhaust a)]

instance RunMessage TheMusclePracticed where
  runMessage msg a@(TheMusclePracticed attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      byTwo <- selectNone $ EnemyAt (locationWithInvestigator iid) <> not_ (EnemyWithId eid)
      reduceAlarmLevelBy (if byTwo then 2 else 1) (attrs.ability 1) iid
      pure a
    InvestigatorEliminated _ -> pure a
    _ -> TheMusclePracticed <$> liftRunMessage msg attrs
