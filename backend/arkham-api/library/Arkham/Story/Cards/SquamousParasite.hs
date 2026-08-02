module Arkham.Story.Cards.SquamousParasite (squamousParasite) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SquamousParasite = SquamousParasite StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

squamousParasite :: StoryCard SquamousParasite
squamousParasite = story SquamousParasite Cards.squamousParasite

instance RunMessage SquamousParasite where
  runMessage msg s@(SquamousParasite attrs) = runQueueT $ case msg of
    -- No flavor modal: the player reads this side off the flipped card itself.
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_t" :: Text, "Air" :: Text)
      -- "Add this card to the victory display" means this side, which is where the
      -- Victory 1 is printed. The enemy front is the same physical card, so it goes
      -- away entirely rather than being discarded or set aside: a quiet removal so
      -- it leaves no trace in play, in a removed zone, or in the defeat records.
      addToVictory iid attrs
      for_ (storyOtherSide attrs) \case
        EnemyTarget eid -> push $ QuietlyRemoveFromGame (EnemyTarget eid)
        _ -> pure ()
      pure s
    _ -> SquamousParasite <$> liftRunMessage msg attrs
