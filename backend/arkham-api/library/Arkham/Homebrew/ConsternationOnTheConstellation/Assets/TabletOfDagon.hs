module Arkham.Homebrew.ConsternationOnTheConstellation.Assets.TabletOfDagon (tabletOfDagon) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets qualified as Cards

newtype TabletOfDagon = TabletOfDagon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The crate the cult is after, and the hinge of the whole scenario: act 2 advances
when an investigator takes control of it, agenda 2 advances if they do not.
Revelation: take control of it. Investigate at +1 shroud to move a Deep One to a
connecting location.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
tabletOfDagon :: AssetCard TabletOfDagon
tabletOfDagon = asset TabletOfDagon Cards.tabletOfDagon

instance RunMessage TabletOfDagon where
  runMessage msg (TabletOfDagon attrs) = runQueueT $ TabletOfDagon <$> liftRunMessage msg attrs
