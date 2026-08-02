module Arkham.Story.Cards.ErodedFrieze (erodedFrieze) where

import Arkham.Projection (field)
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Story.Types (Field (StoryOtherSide))

newtype ErodedFrieze = ErodedFrieze StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erodedFrieze :: StoryCard ErodedFrieze
erodedFrieze = story ErodedFrieze Cards.erodedFriezeStory

instance RunMessage ErodedFrieze where
  runMessage msg s@(ErodedFrieze attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_e" :: Text, "Darkness" :: Text)
      -- The treachery front is the same physical card, so it leaves play rather
      -- than being discarded alongside this side going to the victory display.
      field StoryOtherSide attrs.id >>= traverse_ \case
        TreacheryTarget tid -> push $ RemoveTreachery tid
        _ -> pure ()
      push $ AddToVictory (Just iid) (toTarget attrs)
      pure s
    _ -> ErodedFrieze <$> liftRunMessage msg attrs
