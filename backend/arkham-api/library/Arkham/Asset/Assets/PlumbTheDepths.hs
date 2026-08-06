module Arkham.Asset.Assets.PlumbTheDepths (plumbTheDepths) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEnds)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)

newtype PlumbTheDepths = PlumbTheDepths AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plumbTheDepths :: AssetCard PlumbTheDepths
plumbTheDepths = asset PlumbTheDepths Cards.plumbTheDepths

instance HasAbilities PlumbTheDepths where
  getAbilities (PlumbTheDepths a) =
    [ controlled a 1 (if a.use #obsession > 0 then NoRestriction else Never)
        $ forced
        $ DiscoveringLastClue #after You YourLocation
    , controlled a 2 (if a.use #obsession == 0 then NoRestriction else Never)
        $ forced taskEnds
    ]

instance RunMessage PlumbTheDepths where
  runMessage msg a@(PlumbTheDepths attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      spendUses (attrs.ability 1) attrs #obsession 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.PlumbTheDepths 1
      pure a
    _ -> PlumbTheDepths <$> liftRunMessage msg attrs
