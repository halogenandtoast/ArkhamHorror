module Arkham.Types.Asset.Cards.ClaspOfBlackOnyx
  ( claspOfBlackOnyx
  , ClaspOfBlackOnyx(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

newtype ClaspOfBlackOnyx = ClaspOfBlackOnyx AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor env ClaspOfBlackOnyx where
  getModifiersFor InHandSource (CardIdTarget cardId) (ClaspOfBlackOnyx attrs)
    | toCardId attrs /= cardId = pure
    $ toModifiers attrs [IncreaseCostOf (CardWithId cardId) 1]
  getModifiersFor _ _ _ = pure []

claspOfBlackOnyx :: AssetCard ClaspOfBlackOnyx
claspOfBlackOnyx = asset ClaspOfBlackOnyx Cards.claspOfBlackOnyx

instance AssetRunner env => RunMessage env ClaspOfBlackOnyx where
  runMessage msg (ClaspOfBlackOnyx attrs) =
    ClaspOfBlackOnyx <$> runMessage msg attrs
