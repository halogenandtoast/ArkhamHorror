module Arkham.Story.Cards.UnderseaParasite (underseaParasite) where

import Arkham.Enemy.Types (Field (EnemyMeta))
import Arkham.ForMovement
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.Projection
import Arkham.Scenarios.TheDrownedQuarter.Helpers (UnderseaParasiteFlip (..))
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UnderseaParasite = UnderseaParasite StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaParasite :: StoryCard UnderseaParasite
underseaParasite = story UnderseaParasite Cards.underseaParasite

instance RunMessage UnderseaParasite where
  runMessage msg s@(UnderseaParasite attrs) = runQueueT $ case msg of
    -- No flavor modal: the player reads this side off the flipped card itself.
    ResolveThisStory iid (is attrs -> True) -> do
      -- The enemy stores why it flipped; this side's text branches on it.
      for_ (storyOtherSide attrs) \case
        EnemyTarget eid -> do
          flippedBy <- fieldMap EnemyMeta (toResultDefault FlippedByLeavingPlay) eid
          case flippedBy of
            FlippedByAttack -> do
              -- "Flip this card and move it to a connecting location (with no
              -- investigators, if able)."
              flipOverBy iid attrs eid
              withLocationOf eid \lid -> do
                connected <- select $ ConnectedTo ForMovement (LocationWithId lid)
                unoccupied <- filterM (selectNone . investigatorAt . LocationWithId) connected
                let destinations = if notNull unoccupied then unoccupied else connected
                lead <- getLead
                chooseOrRunOneM lead $ targets destinations (enemyMoveTo attrs eid)
            FlippedByLeavingPlay -> do
              campaignSpecific "translateGlyph" ("rune_x" :: Text, "Sum" :: Text)
              -- "Add this card to the victory display" means this side, which is
              -- where the Victory 1 is printed. Adding the /enemy/ instead would
              -- pull the entity out from under the Discard queued behind us.
              addToVictory iid attrs
        _ -> pure ()
      pure s
    _ -> UnderseaParasite <$> liftRunMessage msg attrs
