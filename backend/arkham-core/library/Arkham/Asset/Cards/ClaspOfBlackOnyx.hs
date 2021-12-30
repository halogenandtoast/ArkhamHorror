module Arkham.Asset.Cards.ClaspOfBlackOnyx
  ( claspOfBlackOnyx
  , ClaspOfBlackOnyx(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Card
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Source
import Arkham.Target

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
