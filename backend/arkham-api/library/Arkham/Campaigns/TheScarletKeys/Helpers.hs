module Arkham.Campaigns.TheScarletKeys.Helpers where

import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Card.CardCode
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (LocationConcealedCards))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Prelude
import Arkham.Window qualified as Window

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theScarletKeys" a

markTime :: ReverseQueue m => Int -> m ()
markTime = incrementRecordCount Time

removeAllConcealed :: ReverseQueue m => m ()
removeAllConcealed = push Msg.RemoveAllConcealed

exposed :: (ReverseQueue m, HasCardCode c) => InvestigatorId -> c -> m ()
exposed iid c = do
  let ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"
  checkWhen $ Window.CampaignEvent ekey (Just iid) Null
  checkAfter $ Window.CampaignEvent ekey (Just iid) Null

whenExposed :: HasCardCode c => c -> WindowMatcher
whenExposed c = CampaignEvent #when Nothing ekey
 where
  ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"

allConcealedMiniCards :: HasGame m => m [ConcealedCardId]
allConcealedMiniCards = concat <$> selectField LocationConcealedCards Anywhere

placeConcealedCard :: ReverseQueue m => InvestigatorId -> ConcealedCardId -> Placement -> m ()
placeConcealedCard iid c placement = push $ Msg.PlaceConcealedCard iid c placement
