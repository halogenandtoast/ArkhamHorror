module Arkham.Asset.Assets.DoNoHarm (doNoHarm) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCount)
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
    $ [InvestigatorHealed #after dt Anyone (SourceOwnedBy You) | dt <- [#damage, #horror]]
    <> [AssetHealed #after dt #ally (SourceOwnedBy You) | dt <- [#damage, #horror]]

instance HasAbilities DoNoHarm where
  getAbilities (DoNoHarm a) =
    [ controlled a 1 (if a.use Obligation > 0 then NoRestriction else Never)
        $ freeReaction healed
    , controlled a 2 (if a.use Obligation == 0 then NoRestriction else Never)
        $ forced
        $ GameEnds #when
    ]

instance RunMessage DoNoHarm where
  runMessage msg a@(DoNoHarm attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      spendUses (attrs.ability 1) attrs Obligation 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      incrementRecordCount Key.DoNoHarm 1
      pure a
    _ -> DoNoHarm <$> liftRunMessage msg attrs
