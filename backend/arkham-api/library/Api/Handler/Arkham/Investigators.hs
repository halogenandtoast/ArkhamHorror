module Api.Handler.Arkham.Investigators (
  getApiV1ArkhamInvestigatorsR,
) where

import Import

import Api.Handler.Arkham.CustomCards (userCustomCards)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef
import Arkham.Card.CardType (CardType (InvestigatorType))
import Arkham.Card.CustomCard (CustomCard (..))
import Arkham.Investigator.Cards
import Data.Map.Strict qualified as Map

{- | Every investigator a deck may be built around.

The printed ones are the same for everybody. A signed-in caller also gets their
own custom investigators -- their library, which is their own plus every set
they are subscribed to -- because the client gates a deck on this list before it
will load one, and without them a custom investigator reads as not implemented
with the card sitting right there.

Codes, not just a code: 'userCustomCards' is keyed by every code a card answers
to, so an investigator that claims an arkham.build id appears under that id's
derived code too -- which is the one a deck built there names it by.

Anonymous callers get the printed list, which is what this route has always
answered. The built-ins go first so the common answer is unchanged in order.
-}
getApiV1ArkhamInvestigatorsR :: Handler [Text]
getApiV1ArkhamInvestigatorsR = do
  custom <- maybe (pure []) customInvestigators =<< lookupRequestUserId
  pure $ map cdArt (toList allInvestigatorCards) <> custom

customInvestigators :: UserId -> Handler [Text]
customInvestigators userId =
  map (unCardCode . fst)
    . filter (isInvestigator . snd)
    . Map.toList
    <$> userCustomCards userId
 where
  isInvestigator = (== InvestigatorType) . cdCardType . customCardDef
