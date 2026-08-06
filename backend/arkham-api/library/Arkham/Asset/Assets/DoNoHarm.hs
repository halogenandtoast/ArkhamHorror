module Arkham.Asset.Assets.DoNoHarm (doNoHarm) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEnds)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)
import Arkham.Token

newtype DoNoHarm = DoNoHarm AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doNoHarm :: AssetCard DoNoHarm
doNoHarm = asset DoNoHarm Cards.doNoHarm

-- After you heal damage/horror from an investigator or Ally asset.
healed :: WindowMatcher
healed =
  oneOf
    $ [InvestigatorHealed #after dt Anyone (SourceUsedBy You) | dt <- [#damage, #horror]]
    <> [AssetHealed #after dt #ally (SourceUsedBy You) | dt <- [#damage, #horror]]

instance HasAbilities DoNoHarm where
  getAbilities (DoNoHarm a) =
    [ controlled a 1 (if a.use Obligation > 0 then NoRestriction else Never)
        $ freeReaction healed
    , controlled a 2 (if a.use Obligation == 0 then NoRestriction else Never)
        $ forced taskEnds
    ]

instance RunMessage DoNoHarm where
  runMessage msg a@(DoNoHarm attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      spendUses (attrs.ability 1) attrs Obligation 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.DoNoHarm 1
      pure a
    _ -> DoNoHarm <$> liftRunMessage msg attrs
