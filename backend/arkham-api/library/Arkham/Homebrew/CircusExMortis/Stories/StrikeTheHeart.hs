module Arkham.Homebrew.CircusExMortis.Stories.StrikeTheHeart (strikeTheHeart) where

import Arkham.Enemy.Creation (createExhausted)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Import.Lifted

{- | Destiny "heart" (:201). The front is the vision Thea's tome deals this investigator;
its one instruction is "Flip this card over and put it into play at Fallen Copse,
exhausted", so resolving it brings the Malformed Dark Young (:201b) into play.
-}
newtype StrikeTheHeart = StrikeTheHeart StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strikeTheHeart :: StoryCard StrikeTheHeart
strikeTheHeart = story StrikeTheHeart Cards.strikeTheHeart

instance RunMessage StrikeTheHeart where
  runMessage msg s@(StrikeTheHeart attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      copse <- selectJust $ locationIs Locations.fallenCopse
      card <- fetchCard Enemies.malformedDarkYoung
      createEnemyWith_ card (AtLocation copse) createExhausted
      pure s
    _ -> StrikeTheHeart <$> liftRunMessage msg attrs
