module Arkham.Asset.Assets.NoPlaceLikeHome (noPlaceLikeHome) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealLocation)
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEnds)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)
import Arkham.Token

newtype NoPlaceLikeHome = NoPlaceLikeHome AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noPlaceLikeHome :: AssetCard NoPlaceLikeHome
noPlaceLikeHome = asset NoPlaceLikeHome Cards.noPlaceLikeHome

-- When you reveal a location or put a location into play (errata Jul 11, 2025).
-- PutLocationIntoPlay only opens an #after window, so both use #after.
revealedOrPutIntoPlay :: WindowMatcher
revealedOrPutIntoPlay =
  oneOf [RevealLocation #after You Anywhere, PutLocationIntoPlay #after You Anywhere]

instance HasAbilities NoPlaceLikeHome where
  getAbilities (NoPlaceLikeHome a) =
    [ controlled a 1 (if a.use Discovery > 0 then NoRestriction else Never)
        $ forced revealedOrPutIntoPlay
    , -- "1 [per_investigator] or fewer discoveries" scales with the player
      -- count, so the threshold has to be a calculation, not a fixed 1.
      controlled
        a
        2
        (HasCalculation (AssetTokenCountCalculation a.id #discovery) (AtMost $ PerPlayer 1))
        $ forced taskEnds
    ]

instance RunMessage NoPlaceLikeHome where
  runMessage msg a@(NoPlaceLikeHome attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      spendUses (attrs.ability 1) attrs Discovery 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.NoPlaceLikeHome 1
      pure a
    _ -> NoPlaceLikeHome <$> liftRunMessage msg attrs
