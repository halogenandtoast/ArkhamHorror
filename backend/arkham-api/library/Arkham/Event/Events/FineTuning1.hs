module Arkham.Event.Events.FineTuning1 (fineTuning1) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Modifier

newtype FineTuning1 = FineTuning1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineTuning1 :: EventCard FineTuning1
fineTuning1 = event FineTuning1 Cards.fineTuning1

instance HasAbilities FineTuning1 where
  getAbilities (FineTuning1 a) =
    [ controlled a 1 (youExist (InvestigatorWithoutModifier ControlledAssetsCannotReady))
        $ triggered
          (Exhausts #after Anyone (AssetTargetMatches $ AssetWithAttachedEvent (be a)))
          (exhaust a)
    ]

instance RunMessage FineTuning1 where
  runMessage msg e@(FineTuning1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <-
        getUpgradeTargets iid
          $ assetControlledBy iid
          <> oneOf [#tool, #science]
          <> not_ (AssetWithAttachedEvent $ eventIs Cards.fineTuning1)
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToAsset aid _ -> readyThis aid
        _ -> pure ()
      pure e
    _ -> FineTuning1 <$> liftRunMessage msg attrs
