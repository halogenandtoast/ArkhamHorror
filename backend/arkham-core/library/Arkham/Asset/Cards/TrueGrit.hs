module Arkham.Asset.Cards.TrueGrit
  ( trueGrit
  , TrueGrit(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Projection
import Arkham.Target

newtype TrueGrit = TrueGrit AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueGrit :: AssetCard TrueGrit
trueGrit = assetWith TrueGrit Cards.trueGrit (healthL ?~ 3)

instance HasModifiersFor TrueGrit where
  getModifiersFor (InvestigatorTarget iid) (TrueGrit a)
    | not (controlledBy a iid) = do
      locationId <- field InvestigatorLocation iid
      assetLocationId <- field AssetLocation (toId a)
      pure
        [ toModifier a (CanAssignDamageToAsset $ toId a)
        | (locationId == assetLocationId) && isJust locationId
        ]
  getModifiersFor _ _ = pure []

instance RunMessage TrueGrit where
  runMessage msg (TrueGrit attrs) = TrueGrit <$> runMessage msg attrs
