module Arkham.Asset.Assets.TheThiefPracticed (theThiefPracticed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (getEnemy)
import Arkham.Matcher hiding (InvestigatorEliminated)
import Arkham.Modifier
import Arkham.Trait (Trait (Casino))

newtype TheThiefPracticed = TheThiefPracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThiefPracticed :: AssetCard TheThiefPracticed
theThiefPracticed = asset TheThiefPracticed Cards.theThiefPracticed

instance HasAbilities TheThiefPracticed where
  getAbilities (TheThiefPracticed a) =
    [ controlled_ a 1
        $ triggered
          ( WouldPatrol #when
              $ EnemyWithTrait Casino
              <> EnemyAt
                (LocationWithDistanceFromAtMost 2 (LocationWithInvestigator (ControlsAsset (be a))) Anywhere)
          )
          (exhaust a)
    ]

instance RunMessage TheThiefPracticed where
  runMessage msg a@(TheThiefPracticed attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      phaseModifier (attrs.ability 1) enemy (ScenarioModifier "reverseDirection")
      pure a
    InvestigatorEliminated _ -> pure a
    _ -> TheThiefPracticed <$> liftRunMessage msg attrs
