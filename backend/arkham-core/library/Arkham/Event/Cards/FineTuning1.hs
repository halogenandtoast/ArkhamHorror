module Arkham.Event.Cards.FineTuning1 (fineTuning1, FineTuning1 (..)) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement

newtype FineTuning1 = FineTuning1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineTuning1 :: EventCard FineTuning1
fineTuning1 = event FineTuning1 Cards.fineTuning1

instance HasAbilities FineTuning1 where
  getAbilities (FineTuning1 a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> youExist (InvestigatorWithoutModifier ControlledAssetsCannotReady))
        $ ReactionAbility
          (Exhausts #after Anyone (AssetTargetMatches $ AssetWithAttachedEvent (be a)))
          (exhaust a)
    ]

instance RunMessage FineTuning1 where
  runMessage msg e@(FineTuning1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs
        $ assetControlledBy iid
        <> oneOf [#tool, #science]
        <> not_ (AssetWithAttachedEvent $ eventIs Cards.tinker)
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      push $ PlaceEvent iid attrs.id $ AttachedToAsset aid Nothing
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToAsset aid _ -> push $ Ready (toTarget aid)
        _ -> pure ()
      pure e
    _ -> FineTuning1 <$> liftRunMessage msg attrs
