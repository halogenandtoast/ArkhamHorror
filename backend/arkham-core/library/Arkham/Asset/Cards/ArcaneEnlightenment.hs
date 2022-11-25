module Arkham.Asset.Cards.ArcaneEnlightenment
  ( ArcaneEnlightenment(..)
  , arcaneEnlightenment
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Target
import Arkham.Trait

newtype ArcaneEnlightenment = ArcaneEnlightenment AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneEnlightenment :: AssetCard ArcaneEnlightenment
arcaneEnlightenment = asset ArcaneEnlightenment Cards.arcaneEnlightenment

instance HasModifiersFor ArcaneEnlightenment where
  getModifiersFor (InvestigatorTarget iid) (ArcaneEnlightenment attrs) =
    pure [ toModifier attrs (HandSize 1) | controlledBy attrs iid ]
  getModifiersFor _ _ = pure []

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance RunMessage ArcaneEnlightenment where
  runMessage msg (ArcaneEnlightenment attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push (AddSlot iid HandSlot (slot attrs))
      ArcaneEnlightenment <$> runMessage msg attrs
    _ -> ArcaneEnlightenment <$> runMessage msg attrs
