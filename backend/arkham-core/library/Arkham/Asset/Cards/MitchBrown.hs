module Arkham.Asset.Cards.MitchBrown
  ( mitchBrown
  , MitchBrown(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype MitchBrown = MitchBrown AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mitchBrown :: AssetCard MitchBrown
mitchBrown = ally MitchBrown Cards.mitchBrown (2, 2)

slot :: AssetAttrs -> Slot
slot attrs = RestrictedSlot (toSource attrs) (NotCard CardIsUnique) Nothing

instance RunMessage MitchBrown where
  runMessage msg (MitchBrown attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    PlayedCard iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid AllySlot (slot attrs))
      MitchBrown <$> runMessage msg attrs
    _ -> MitchBrown <$> runMessage msg attrs
