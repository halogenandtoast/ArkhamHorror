module Arkham.Types.Asset.Cards.AshleighClarke
  ( ashleighClarke
  , AshleighClarke(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype AshleighClarke = AshleighClarke AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashleighClarke :: AssetCard AshleighClarke
ashleighClarke = asset AshleighClarke Cards.ashleighClarke

instance AssetRunner env => RunMessage env AshleighClarke where
  runMessage msg (AshleighClarke attrs) =
    AshleighClarke <$> runMessage msg attrs
