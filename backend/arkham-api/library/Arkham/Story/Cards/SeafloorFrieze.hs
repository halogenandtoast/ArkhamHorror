module Arkham.Story.Cards.SeafloorFrieze (seafloorFrieze) where

import Arkham.Projection (field)
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Story.Types (Field (StoryOtherSide))

newtype SeafloorFrieze = SeafloorFrieze StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seafloorFrieze :: StoryCard SeafloorFrieze
seafloorFrieze = story SeafloorFrieze Cards.seafloorFrieze

instance RunMessage SeafloorFrieze where
  runMessage msg s@(SeafloorFrieze attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_w" :: Text, "Parasite" :: Text)
      field StoryOtherSide attrs.id >>= traverse_ \case
        TreacheryTarget tid -> push $ RemoveTreachery tid
        _ -> pure ()
      push $ AddToVictory (Just iid) (toTarget attrs)
      pure s
    _ -> SeafloorFrieze <$> liftRunMessage msg attrs
