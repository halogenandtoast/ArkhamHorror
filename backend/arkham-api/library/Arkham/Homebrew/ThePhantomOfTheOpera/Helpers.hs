{- | Shared rules for The Phantom of the Opera.

The whole scenario runs on the Scarlet Keys concealed system: the Phantom and
his four minions sit in the shadows as mini-cards alongside the decoys, and the
locations and acts are almost entirely about looking at, moving and exposing
them. "X" on Terrible Guise and Violent Shade is the count of mini-cards at
their own location, capped at 5.
-}
module Arkham.Homebrew.ThePhantomOfTheOpera.Helpers where

import Arkham.Campaigns.TheScarletKeys.Concealed (mkConcealedCard)
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (distributeEvenlyBetween)
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind (ConcealedCardKind (Decoy))
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Concealed.Query (ForExpose (NotForExpose), getConcealedAtAll)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.ThePhantomOfTheOpera.Kinds qualified as Kinds
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (CreateConcealedCard))
import Arkham.Message.Lifted
import Arkham.Modifier
import Arkham.Prelude

concealedAt :: LocationId -> ConcealedCardMatcher
concealedAt lid = ConcealedCardAt (LocationWithId lid)

{- | "Concealed mini-cards cannot be placed at <location>." 'placeConcealed'
filters on a per-kind @noConcealed[<kind>]@ campaign modifier, so blocking a
location outright means naming every kind this scenario can place -- the
Sanguine Shadows / On Thin Ice shape, published by the scenario.
-}
noConcealedPlacement :: [ModifierType]
noConcealedPlacement =
  [ CampaignModifier ("noConcealed[" <> tshow kind <> "]")
  | kind <-
      Decoy
        : [ Kinds.ThePhantom
          , Kinds.BuriedSorrows
          , Kinds.SimmeringAnger
          , Kinds.StifledLaughter
          , Kinds.ChaoticMinionA
          , Kinds.ChaoticMinionB
          ]
  ]

-- | "X is equal to the number of concealed mini-cards at <enemy>'s location (max 5)."
concealedCountAt :: HasGame m => LocationId -> m Int
concealedCountAt lid = min 5 . length <$> getConcealedAtAll NotForExpose lid

{- | "If there are fewer than 2[per_investigator] total concealed mini-cards in
play, add additional decoys until that number is met."

MUST resolve in an earlier queue step than 'redistributeConcealed': a pushed
'CreateConcealedCard' is not an entity until the queue processes it, so calling
the two back to back leaves the new decoys 'Unplaced' and undealt.
-}
topUpConcealedDecoys :: ReverseQueue m => m ()
topUpConcealedDecoys = do
  required <- perPlayer 2
  inPlay <- selectCount ConcealedCardAny
  for_ [1 .. required - inPlay] \_ -> push . CreateConcealedCard =<< mkConcealedCard Decoy

{- | "Shuffle each concealed mini-card in play and redistribute them between
each <X> location equally." Three act backs and Hidden Parlor print it. The
'noConcealedPlacement' exclusion lives in here, or a room that forbids
mini-cards would be dealt them anyway.
-}
redistributeConcealed :: ReverseQueue m => LocationMatcher -> m ()
redistributeConcealed matcher = do
  locations <- select $ matcher <> foldMap LocationWithoutModifier noConcealedPlacement
  concealed <- shuffle =<< select ConcealedCardAny
  when (notNull locations) $ distributeEvenlyBetween concealed locations

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "thePhantomOfTheOpera" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a
