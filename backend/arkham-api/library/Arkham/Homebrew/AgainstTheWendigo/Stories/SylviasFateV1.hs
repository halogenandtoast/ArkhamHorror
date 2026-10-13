module Arkham.Homebrew.AgainstTheWendigo.Stories.SylviasFateV1 (sylviasFateV1) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Location.Grid (Pos (..))
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Projection (field)
import Arkham.Story.Import.Lifted

newtype SylviasFateV1 = SylviasFateV1 StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. Here the
forest itself took Sylvia; the card flips into The Heart of the Forest, whose
revelation drags everyone in the Impenetrable Forest into it.
-}
sylviasFateV1 :: StoryCard SylviasFateV1
sylviasFateV1 = persistStory $ story SylviasFateV1 Cards.sylviasFateV1

{- | "Put The Heart of the Forest into play" beside the Impenetrable Forest,
which the scenario deals at random into one of the valley's two outer columns.
One cell further out from the river, so it lands on whichever side the Forest
ended up on.
-}
besideTheForest :: ReverseQueue m => m Pos
besideTheForest = do
  mpos <- selectOne (locationIs Locations.impenetrableForest) >>= traverse (field LocationPosition)
  pure $ case join mpos of
    Just (Pos x y) -> Pos (if x < 0 then x - 1 else x + 1) y
    Nothing -> Pos 2 0

instance RunMessage SylviasFateV1 where
  runMessage msg s@(SylviasFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      selectForMaybeM (locationIs Locations.impenetrableForest) reveal
      -- Put aside, but still on the table: the Impenetrable Forest's reaction reads
      -- the second part by resolving this card again.
      pure $ SylviasFateV1 $ attrs & placementL .~ Global
    -- The second part, asked for by the Impenetrable Forest once its clues run out.
    ResolveThisStory _ (is attrs -> True) -> do
      record YouHaveDiscoveredSylviasFate
      {- `unsafeReveal`, not `reveal`: that one asks whether its location is an
      UnrevealedLocation before pushing anything, and a location only queued to
      be placed is not in play to be asked about, so the reveal was dropped. -}
      -- The story is the back's own card, so it leaves before the back arrives.
      removeStory attrs
      pos <- besideTheForest
      unsafeReveal =<< placeLocationCardInGrid pos Locations.theHeartOfTheForest
      pure s
    _ -> SylviasFateV1 <$> liftRunMessage msg attrs
