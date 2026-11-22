module Arkham.Helpers.Dreaming.Lifted (
  flippableAssets,
  flipAnAssetFacedown,
  dreamOrElse,
  dreamMoveOptions,
  moveDreamBetweenDreamingCards,
) where

import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Dreaming (DreamPlace, getDreamMoves)
import Arkham.Helpers.Message (push)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted (moveTokens)
import Arkham.Message.Lifted.Choose (
  ChooseT,
  chooseOneM,
  chooseOrRunOneM,
  labeled,
  questionLabeledI,
  targeting,
  targets,
 )
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Queue (QueueT)
import Arkham.Source
import Arkham.Target
import Arkham.Token

{- | "A non-weakness, non-story asset you control", the selection the five
flip-facedown cards share (14011, 14045, 14057, 14060, 14067).

'AssetCanLeavePlayByNormalMeans' is a rider rather than printed text: the flip
makes the asset LEAVE PLAY, so a 'CannotLeavePlay'-style ban has to bite here
the way it does on a return-to-hand.
-}
flippableAssets :: InvestigatorId -> AssetMatcher
flippableAssets iid =
  assetControlledBy iid
    <> NonWeaknessAsset
    <> AssetNonStory
    <> AssetCanLeavePlayByNormalMeans

{- | "Flip a non-weakness, non-story asset you control facedown and place @n@
resources on it, as dreams."

Not a "you may", so there is no decline option and a lone candidate resolves
without an ask. 'FlipAssetFacedown' owns everything past the choice: the asset
leaves play (so damage, uses and attachments go with it and the slot frees), and
at the dreaming-card control cap the flip does nothing and the asset stays put.
-}
flipAnAssetFacedown :: ReverseQueue m => InvestigatorId -> Int -> m ()
flipAnAssetFacedown iid n = do
  assets <- select $ flippableAssets iid
  chooseOrRunOneM iid $ targets assets \aid -> push $ FlipAssetFacedown iid aid n

{- | "You must either place 1 dream on a dreaming card you control, or
@fallback@." Shared by Lunar Sight (14054) and Reality Fracture (14055), whose
only difference is what the fallback does.

The ask survives down to one option on purpose. Controlling no dreaming card
deletes the first branch, and a 'chooseOrRunOneM' would then resolve the
fallback unasked -- the investigator takes the damage and loses the card with
nothing saying the other half was never available (#5998, #6017).
-}
dreamOrElse
  :: (HasI18n, ReverseQueue m, Sourceable source)
  => InvestigatorId
  -> source
  -> Text
  -> QueueT Message m ()
  -> m ()
dreamOrElse iid source fallbackLabel fallback = do
  dreamingCards <- select $ DreamingCardControlledBy (InvestigatorWithId iid)
  chooseOneM iid do
    targets dreamingCards \dreamingCard ->
      push $ PlaceTokens (toSource source) (toTarget dreamingCard) Dream 1
    labeled fallbackLabel fallback

{- | The source-then-destination asks for such a move, as options, so a card that
prints "you may" can offer its own decline beside them.

The move is one 'MoveTokens', which bridges both ends. When the source is a
dreaming card that routes the removal through the single dream-removal point, so
taking its last dream flips it faceup at once, mid-turn. Never hand-roll that
check.
-}
dreamMoveOptions
  :: (ReverseQueue m, Sourceable source)
  => InvestigatorId
  -> source
  -> [(DreamPlace, [DreamPlace])]
  -> ChooseT m ()
dreamMoveOptions iid source moves = do
  questionLabeledI "chooseDreamingCardToMoveDreamFrom"
  for_ moves \(from, tos) ->
    targeting from $ chooseOneM iid do
      questionLabeledI "chooseDreamingCardToMoveDreamTo"
      targets tos \dest -> moveTokens source from dest Dream 1

-- | 'dreamMoveOptions' as a whole ask. Does nothing when no legal move exists.
moveDreamBetweenDreamingCards
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
moveDreamBetweenDreamingCards iid source = do
  moves <- getDreamMoves iid
  when (notNull moves) $ chooseOneM iid $ dreamMoveOptions iid source moves
