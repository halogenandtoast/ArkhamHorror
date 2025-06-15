module Arkham.Location.Cards.TMGArtGallery (tmgArtGallery) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import

newtype TMGArtGallery = TMGArtGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Art Gallery' from The Midwinter Gala (#71009).
tmgArtGallery :: LocationCard TMGArtGallery
tmgArtGallery =
  location
    TMGArtGallery
    Cards.tmgArtGallery
    3
    (PerPlayer 1)
    Triangle
    [Diamond, Spade, Moon, Square]
    & revealedBy False

instance HasAbilities TMGArtGallery where
  getAbilities (TMGArtGallery attrs) =
    withBaseAbilities attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility Nothing
      ]

instance RunMessage TMGArtGallery where
  runMessage msg l@(TMGArtGallery attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: Implement clue placement and resource gain
      pure l
    _ -> TMGArtGallery <$> runMessage msg attrs
