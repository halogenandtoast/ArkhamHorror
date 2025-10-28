module Arkham.Event.Events.AdHoc (adHoc) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Ability (getCanPerformAbility)
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype AdHoc = AdHoc EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adHoc :: EventCard AdHoc
adHoc = event AdHoc Cards.adHoc

instance HasAbilities AdHoc where
  getAbilities (AdHoc a) =
    [ controlled_ a 1
        $ triggered (ActivateAbility #after You $ AbilityOnAsset (AssetWithAttachedEvent (be a)))
        $ exhaust a
        <> HandDiscardCost
          1
          (#asset <> oneOf [#tool, #weapon] <> CardWithPerformableAbility #action [IgnoreAllCosts])
    ]

instance RunMessage AdHoc where
  runMessage msg e@(AdHoc attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      upgradeTargets <- getUpgradeTargets iid $ assetControlledBy iid <> oneOf [#tool, #weapon]
      chooseOneToHandle iid attrs upgradeTargets
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (AssetTarget aid) -> do
      push $ PlaceEvent attrs.id (AttachedToAsset aid Nothing)
      pure e
    UseCardAbility _iid (isSource attrs -> True) 1 _ (discardedCards -> [card]) -> do
      withCardEntity @AssetId card (`forTarget` msg)
      pure e
    ForTarget (AssetTarget aid) (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let adjustAbility ab = applyAbilityModifiers (noAOO ab) [IgnoreAllCosts]
      abilities <- selectMap adjustAbility $ AssetAbility (AssetWithId aid) <> #action
      abilities' <- filterM (getCanPerformAbility iid (defaultWindows iid)) abilities
      chooseOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities']
      pure e
    _ -> AdHoc <$> liftRunMessage msg attrs
