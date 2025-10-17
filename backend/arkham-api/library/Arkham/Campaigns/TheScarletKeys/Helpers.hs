module Arkham.Campaigns.TheScarletKeys.Helpers
  ( module Arkham.Campaigns.TheScarletKeys.Helpers
  , module Arkham.Campaigns.TheScarletKeys.I18n
  )
  where

import Arkham.Campaigns.TheScarletKeys.I18n
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Types (Field (..), ScarletKeyId)
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Effect.Window
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Id
import Arkham.Location.Types (Field (LocationConcealedCards))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Window qualified as Window

markTime :: ReverseQueue m => Int -> m ()
markTime = incrementRecordCount Time

getTime :: ReverseQueue m => m Int
getTime = getRecordCount Time

removeAllConcealed :: ReverseQueue m => m ()
removeAllConcealed = push Msg.RemoveAllConcealed

exposed :: (ReverseQueue m, HasCardCode c, ToId enemy EnemyId) => InvestigatorId -> enemy -> c -> m () -> m ()
exposed iid enemy c body = do
  let ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"
  let ikey = "exposed[" <> tshow (asId enemy) <> "]"
  checkWhen $ Window.CampaignEvent ekey (Just iid) Null
  checkWhen $ Window.CampaignEvent ikey (Just iid) Null
  checkWhen $ Window.CampaignEvent "exposed[enemy]" (Just iid) Null
  body
  checkAfter $ Window.CampaignEvent ekey (Just iid) Null
  checkAfter $ Window.CampaignEvent ikey (Just iid) Null
  checkAfter $ Window.CampaignEvent "exposed[enemy]" (Just iid) Null

exposedDecoy :: ReverseQueue m => InvestigatorId -> m ()
exposedDecoy iid = do
  let ekey = "exposed[decoy]"
  checkWhen $ Window.CampaignEvent ekey (Just iid) Null
  checkAfter $ Window.CampaignEvent ekey (Just iid) Null

whenExposed :: HasCardCode c => c -> WindowMatcher
whenExposed c = CampaignEvent #when Nothing ekey
 where
  ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"

afterExposed :: HasCardCode c => c -> WindowMatcher
afterExposed c = CampaignEvent #after Nothing ekey
 where
  ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"

allConcealedMiniCards :: HasGame m => m [ConcealedCardId]
allConcealedMiniCards = concat <$> selectField LocationConcealedCards Anywhere

placeConcealedCard :: ReverseQueue m => InvestigatorId -> ConcealedCardId -> Placement -> m ()
placeConcealedCard iid c placement = push $ Msg.PlaceConcealedCard iid c placement

hollow :: ReverseQueue m => InvestigatorId -> Card -> m ()
hollow iid card = do
  setCardAside card
  createWindowModifierEffect_ (EffectHollowWindow card.id) ScenarioSource iid [Hollow card.id]

setBearer :: ReverseQueue m => CardDef -> KeyStatus -> m ()
setBearer skey sts@(KeyWithInvestigator iid) = do
  addCampaignCardToDeck iid Msg.DoNotShuffleIn skey
  campaignSpecific "setBearer" (skey.cardCode, sts)
setBearer skey sts@(KeyWithEnemy {}) = do
  removeCampaignCard skey
  campaignSpecific "setBearer" (skey.cardCode, sts)

shift :: ReverseQueue m => ScarletKeyId -> m ()
shift skeyId = do
  cCode <- field ScarletKeyCardCode skeyId
  push $ Msg.CampaignSpecific ("shift[" <> unCardCode cCode <> "]") Null

setupKeys :: ReverseQueue m => m ()
setupKeys = do
  skeys <- getCampaignStoryCards
  for_ (mapToList skeys) \(iid, cards) -> do
    for_ cards \card -> do
      when (card.kind == KeyType) do
        createScarletKeyAt_ card (AttachedToInvestigator iid)

chooseBearer :: ReverseQueue m => CardDef -> m ()
chooseBearer def = do
  investigators <- getInvestigators
  leadChooseOneM do
    questionLabeled "Choose bearer"
    questionLabeledCard def
    portraits investigators $ setBearer def . KeyWithInvestigator
