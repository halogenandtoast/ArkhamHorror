module Arkham.Asset.Cards.TrueGrit
  ( trueGrit
  , TrueGrit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field(..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Source

newtype TrueGrit = TrueGrit AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueGrit :: AssetCard TrueGrit
trueGrit = assetWith TrueGrit Cards.trueGrit (healthL ?~ 3)

instance HasModifiersFor TrueGrit where
  getModifiersFor (InvestigatorSource iid) target (TrueGrit a)
    | isTarget a target = do
      case assetPlacement a of
        InPlayArea iid' | iid == iid' -> do
          locationId <- field InvestigatorLocation iid
          assetLocationId <- field AssetLocation (toId a)
          pure
            [ toModifier a CanBeAssignedDamage
            | (locationId == assetLocationId)
              && isJust locationId
            ]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage TrueGrit where
  runMessage msg (TrueGrit attrs) = TrueGrit <$> runMessage msg attrs
