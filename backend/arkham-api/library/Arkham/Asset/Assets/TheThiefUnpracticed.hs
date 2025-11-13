module Arkham.Asset.Assets.TheThiefUnpracticed (theThiefUnpracticed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (getEnemy)
import Arkham.Matcher hiding (InvestigatorEliminated)
import Arkham.Modifier
import Arkham.Trait (Trait (Casino))

newtype TheThiefUnpracticed = TheThiefUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThiefUnpracticed :: AssetCard TheThiefUnpracticed
theThiefUnpracticed = asset TheThiefUnpracticed Cards.theThiefUnpracticed

instance HasAbilities TheThiefUnpracticed where
  getAbilities (TheThiefUnpracticed a) =
    [ controlled_ a 1
        $ triggered
          ( WouldPatrol #when
              $ EnemyWithTrait Casino
              <> EnemyAt (connectedFrom $ LocationWithInvestigator (ControlsAsset (be a)))
          )
          (exhaust a)
    ]

instance RunMessage TheThiefUnpracticed where
  runMessage msg a@(TheThiefUnpracticed attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      phaseModifier (attrs.ability 1) enemy (ScenarioModifier "reverseDirection")
      pure a
    InvestigatorEliminated _ -> pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theThiefPracticed
      pure a
    _ -> TheThiefUnpracticed <$> liftRunMessage msg attrs
