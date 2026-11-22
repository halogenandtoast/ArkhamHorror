module Arkham.Helpers.Dreaming where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.DreamingCard
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Token

{- | The dreaming value of a card, or 'Nothing' when it has none.

THE one resolver. No other call site may read @cdKeywords@ for dreaming: a
printed-only read makes a card that GRANTS dreaming, and one that REDUCES it,
silently inert -- the @cdSkills@/@iconsForCard@ bug class (#5972).
-}
getDreamingValue :: HasGame m => Card -> m (Maybe Int)
getDreamingValue card = do
  mods <- getModifiers card
  pure $ dreamingValueWith mods card

{- | 'getDreamingValue' for a caller that already holds the card's modifiers, so
the cost and playability paths do not fetch them twice. Same formula.
-}
dreamingValueWith :: [ModifierType] -> Card -> Maybe Int
dreamingValueWith mods card =
  case printed <> granted of
    [] -> Nothing
    ns -> Just $ max 0 (foldr max 0 ns - reduction)
 where
  printed = [n | Keyword.Dreaming n <- setToList (toCardDef card).keywords]
  granted = [n | AddKeyword (Keyword.Dreaming n) <- mods]
  reduction = sum [n | ReduceDreaming n <- mods]

-- | Whether a card is dreamed into play rather than paid for with resources.
isDreamingCard :: HasGame m => Card -> m Bool
isDreamingCard = fmap isJust . getDreamingValue

{- | How many dreaming cards an investigator may control at once. The literal 3
and the 'AdditionalDreamingCard' fold live here and nowhere else.
-}
getDreamingCardLimit :: HasGame m => InvestigatorId -> m Int
getDreamingCardLimit iid = do
  mods <- getModifiers iid
  pure $ 3 + sum [n | AdditionalDreamingCard n <- mods]

-- | A hard cap, the way a slot limit is: at the limit nothing new can be dreamed.
canCreateDreamingCard :: HasGame m => InvestigatorId -> m Bool
canCreateDreamingCard iid = do
  limit <- getDreamingCardLimit iid
  held <- field InvestigatorDreamingCards iid
  pure $ length held < limit

getDreamingCards :: HasGame m => InvestigatorId -> m [DreamingCard]
getDreamingCards = field InvestigatorDreamingCards

getDreamingCard :: HasGame m => InvestigatorId -> CardId -> m (Maybe DreamingCard)
getDreamingCard iid cid = find ((== cid) . (.id)) <$> getDreamingCards iid

{- | Somewhere dreams may be moved to or from: a facedown dreaming card, or an
in-play asset an 'IsDreamEndpoint' modifier makes one of -- "Dreams may be moved
to and from Idol of Bokrug as if it were a dreaming card under your control"
(14028).

An endpoint is a dreaming card for MOVEMENT and for nothing else, which is why it
is deliberately not a 'DreamingCard': it cannot reach the control limit, Sigil of
Thalarion's "control a dreaming card with 5 or more dreams", Dream Mastery or
Veil of Thalarion. Placing a dream and removing one are not moving one, so those
stay out of this too.
-}
data DreamPlace
  = DreamedCard DreamingCard
  | -- | An endpoint, its dreams, and the most it may hold at once.
    DreamEndpoint AssetId Int (Maybe Int)
  deriving stock (Show, Eq)

instance Targetable DreamPlace where
  toTarget (DreamedCard dc) = toTarget dc
  toTarget (DreamEndpoint aid _ _) = toTarget aid

instance Sourceable DreamPlace where
  toSource (DreamedCard dc) = toSource dc
  toSource (DreamEndpoint aid _ _) = toSource aid

dreamPlaceDreams :: DreamPlace -> Int
dreamPlaceDreams (DreamedCard dc) = dc.dreams
dreamPlaceDreams (DreamEndpoint _ n _) = n

-- | Enforced where a destination is OFFERED, so an over-capacity move is never presented.
dreamPlaceHasRoom :: DreamPlace -> Bool
dreamPlaceHasRoom p = maybe True (dreamPlaceDreams p <) (capacity p)
 where
  capacity (DreamedCard _) = Nothing
  capacity (DreamEndpoint _ _ cap) = cap

-- | Every place this investigator may move dreams to or from.
getDreamPlaces :: HasGame m => InvestigatorId -> m [DreamPlace]
getDreamPlaces iid = do
  cards <- map DreamedCard <$> getDreamingCards iid
  assets <- select $ Matcher.AssetControlledBy (Matcher.InvestigatorWithId iid)
  endpoints <- forMaybeM assets \aid -> do
    mods <- getModifiers aid
    for (listToMaybe [cap | IsDreamEndpoint cap <- mods]) \cap -> do
      dreams <- fieldMap AssetTokens (countTokens Dream) aid
      pure $ DreamEndpoint aid dreams cap
  pure $ cards <> endpoints

{- | Every legal "move 1 dream from X to Y you control": a place holding at least
one dream, paired with the other places it may move one to.
-}
getDreamMoves :: HasGame m => InvestigatorId -> m [(DreamPlace, [DreamPlace])]
getDreamMoves iid = do
  places <- getDreamPlaces iid
  pure
    [ (from, tos)
    | from <- places
    , dreamPlaceDreams from > 0
    , let tos = filter (\dest -> toTarget dest /= toTarget from && dreamPlaceHasRoom dest) places
    , notNull tos
    ]

canMoveADream :: HasGame m => InvestigatorId -> m Bool
canMoveADream = fmap notNull . getDreamMoves

{- | Every place that could move a dream TO @destination@: it holds a dream, and
it is not the destination itself -- moving a dream from a place to that same
place is not a move. Any OTHER endpoint is a legal source, which is why this
generalises rather than naming the dream zone.

The criterion ('Arkham.Criteria.CanMoveADreamTo') is derived from this same
predicate, so the gate and the list it gates cannot disagree.
-}
getDreamSourcesFor :: HasGame m => Target -> InvestigatorId -> m [DreamPlace]
getDreamSourcesFor destination iid =
  filter (\p -> toTarget p /= destination && dreamPlaceDreams p > 0) <$> getDreamPlaces iid

canMoveADreamTo :: HasGame m => Target -> InvestigatorId -> m Bool
canMoveADreamTo destination iid = do
  places <- getDreamPlaces iid
  let hasRoom = any dreamPlaceHasRoom $ filter ((== destination) . toTarget) places
  sources <- getDreamSourcesFor destination iid
  pure $ hasRoom && notNull sources
