module Arkham.Story.Cards.GlyphOrrery (glyphOrrery) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype GlyphOrrery = GlyphOrrery StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glyphOrrery :: StoryCard GlyphOrrery
glyphOrrery = story GlyphOrrery Cards.glyphOrreryStory

instance RunMessage GlyphOrrery where
  runMessage msg s@(GlyphOrrery attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_d" :: Text, "Wave" :: Text)
      pure s
    _ -> GlyphOrrery <$> liftRunMessage msg attrs
