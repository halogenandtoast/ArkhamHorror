{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Campaigns.TheScarletKeys.Helpers (
  module Arkham.Campaigns.TheScarletKeys.Helpers,
  module Arkham.Campaigns.TheScarletKeys.I18n,
)
where

import Arkham.Campaign.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Campaigns.TheScarletKeys.I18n
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types (Field (..), ScarletKeyId)
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Campaigns.TheScarletKeys.Modifiers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign (getCampaignMeta, getCampaignStoryCards)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid
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
import Arkham.Scenario.Setup (ScenarioBuilderT, addToEncounterDeck)
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.Window qualified as Window
import Arkham.Xp

pattern HollowedCard :: ExtendedCardMatcher
pattern HollowedCard <- CardWithModifier (CampaignModifier "hollowed")
  where
    HollowedCard = CardWithModifier (CampaignModifier "hollowed")

pattern EnemyWithAnyScarletKey :: EnemyMatcher
pattern EnemyWithAnyScarletKey <- EnemyWithScarletKey ScarletKeyAny
  where
    EnemyWithAnyScarletKey = EnemyWithScarletKey ScarletKeyAny

pattern InvestigatorWithAnyScarletKey :: InvestigatorMatcher
pattern InvestigatorWithAnyScarletKey <- InvestigatorWithScarletKey ScarletKeyAny
  where
    InvestigatorWithAnyScarletKey = InvestigatorWithScarletKey ScarletKeyAny

data LocationsInShadowsMetadata = LocationsInShadowsMetadata
  { locationsInShadows :: LocationsInShadows
  , concealedCards :: Map Pos [ConcealedCardId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LocationsInShadows = LocationsInShadows
  { left :: Maybe Card
  , middle :: Maybe Card
  , right :: Maybe Card
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data StatusReport = Alpha | Beta | Epsilon | Zeta | Gamma | Theta | Psi | Omega
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

markTime :: ReverseQueue m => Int -> m ()
markTime n = do
  t <- getTime
  meta <- getCampaignMeta @TheScarletKeysMeta
  incrementRecordCount Time n
  let statusEvents =
        sortOn fst
          $ [(7, Alpha), (10, Epsilon), (15, Beta), (20, Zeta), (24, Gamma), (35, Omega)]
          <> [(x, Theta) | x <- maybeToList meta.theta]
          <> [(x, Psi) | x <- maybeToList meta.psi]
  for_ statusEvents \(x, r) ->
    when (t < x && t + n >= x) $ campaignSpecific "statusReport" r

getTime :: ReverseQueue m => m Int
getTime = getRecordCount Time

removeAllConcealed :: ReverseQueue m => m ()
removeAllConcealed = push Msg.RemoveAllConcealed

exposed
  :: (ReverseQueue m, HasCardCode c, ToId enemy EnemyId) => InvestigatorId -> enemy -> c -> m () -> m ()
exposed iid enemy c body = do
  let ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"
  let idkey = "exposed[" <> tshow (asId enemy) <> "]"
  checkWhen $ Window.CampaignEvent ekey (Just iid) Null
  checkWhen $ Window.CampaignEvent idkey (Just iid) Null
  checkWhen $ Window.CampaignEvent "exposed[enemy]" (Just iid) Null
  body
  checkAfter $ Window.CampaignEvent ekey (Just iid) Null
  checkAfter $ Window.CampaignEvent idkey (Just iid) Null
  checkAfter $ Window.CampaignEvent "exposed[enemy]" (Just iid) Null

getCanExpose :: (Tracing m, HasGame m) => InvestigatorId -> ConcealedCard -> m Bool
getCanExpose iid card = runValidT do
  imods <- lift $ getModifiers iid
  guard $ CannotExpose `notElem` imods
  case card.placement of
    AtLocation location -> do
      guard $ noExposeAt location `notElem` imods
      liftGuardM $ matches location $ LocationWithoutModifier NoExposeAt
    _ -> pure ()

exposedDecoy :: ReverseQueue m => InvestigatorId -> ConcealedCard -> Maybe Text -> m ()
exposedDecoy iid card@(toTarget -> c) mtext = do
  whenM (getCanExpose iid card) do
    let ekey = "exposed[decoy]"
    batched \_ -> do
      checkWhen $ Window.CampaignEvent ekey (Just iid) (toJSON c)
      for_ mtext \txt ->
        checkWhen $ Window.CampaignEvent ("exposed[" <> txt <> "]") (Just iid) (toJSON c)
      checkAfter $ Window.CampaignEvent ekey (Just iid) (toJSON c)
      for_ mtext \txt ->
        checkAfter $ Window.CampaignEvent ("exposed[" <> txt <> "]") (Just iid) (toJSON c)
      removeFromGame c

whenExposed :: HasCardCode c => c -> WindowMatcher
whenExposed c = CampaignEvent #when Nothing ekey
 where
  ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"

afterExposed :: HasCardCode c => c -> WindowMatcher
afterExposed c = CampaignEvent #after Nothing ekey
 where
  ekey = "exposed[" <> unCardCode (toCardCode c) <> "]"

allConcealedMiniCards :: (HasGame m, Tracing m) => m [ConcealedCardId]
allConcealedMiniCards = concat <$> selectField LocationConcealedCards Anywhere

placeConcealedCard :: ReverseQueue m => InvestigatorId -> ConcealedCardId -> Placement -> m ()
placeConcealedCard iid c placement = push $ Msg.PlaceConcealedCard iid c placement

hollow :: ReverseQueue m => InvestigatorId -> Card -> m ()
hollow iid card = do
  setCardAside card
  createWindowModifierEffect_ (EffectHollowWindow card.id) ScenarioSource iid [Hollow card.id]
  createWindowModifierEffect_
    (EffectHollowWindow card.id)
    ScenarioSource
    card
    [CampaignModifier "hollowed"]
  checkAfter $ Window.CampaignEvent "hollowed" (Just iid) (toJSON card)

removeHollow :: (FetchCard c, ReverseQueue m) => c -> m ()
removeHollow c = do
  obtainCard c
  fetchCard c >>= scenarioSpecific "removedHollow"

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
  investigators <- allInvestigators
  leadChooseOneM do
    questionLabeled "Choose bearer"
    questionLabeledCard def
    portraits investigators $ setBearer def . KeyWithInvestigator

swapTokens :: (HasI18n, ReverseQueue m) => ChaosTokenFace -> ChaosTokenFace -> m ()
swapTokens face1 face2 = do
  removeChaosToken face1
  selectOne TheCampaign >>= \case
    Nothing -> addChaosToken face2
    Just c -> do
      tknCount <- fieldMap CampaignChaosBag (count (== face2)) c
      if tknCount >= 4
        then do
          let xp = unscoped $ scope "theScarletKeys" $ toBonus "chaosTokens" 1
          eachInvestigator \iid ->
            pushAll
              [ Msg.ReportXp
                  $ XpBreakdown
                    [InvestigatorGainXp iid $ XpDetail XpBonus txt n | WithBonus txt n <- xp.flatten]
              , Msg.GainXP iid CampaignSource xp.value
              ]
        else addChaosToken face2

haven'tSeenTheLastOf :: ReverseQueue m => m [CardDef]
haven'tSeenTheLastOf = do
  let
    redCoterie =
      [ (Enemies.alikiZoniUperetriaSpeaksInDeath, YouHaventSeenTheLastOfAlikiZoniUperetria)
      , (Enemies.amaranthScarletScorn, YouHaventSeenTheLastOfAmaranth)
      , (Enemies.desiderioDelgadoAlvarezRedInHisLedger, YouHaventSeenTheLastOfDesiderioDelgadoAlvarez)
      , (Enemies.laChicaRojaHotOnYourTrail, YouHaventSeenTheLastOfLaChicaRoja)
      , (Enemies.theClaretKnightHoldsYouInContempt, YouHaventSeenTheLastOfTheClaretKnight)
      , (Enemies.theRedGlovedManPurposeUnknown, YouHaventSeenTheLastOfTheRedGlovedMan)
      , (Enemies.theSanguineWatcherHeSeesWhatIsNotThere, YouHaventSeenTheLastOfTheSanguineWatcher)
      , (Enemies.thorneOpenToNegotiation, YouHaventSeenTheLastOfThorne)
      ,
        ( Enemies.theBeastInACowlOfCrimsonLeavingATrailOfDestruction
        , YouHaventSeenTheLastOfTheBeastInACowlOfCrimson
        )
      , (Enemies.tzuSanNiangAWhisperInYourEar, YouHaventSeenTheLastOfTzuSanNiang)
      ]
  map fst <$> (redCoterie & filterM (getHasRecord . snd))

handleRedCoterie :: ReverseQueue m => ScenarioBuilderT m ()
handleRedCoterie = do
  t <- getTime
  when (t >= 10) do
    lift haven'tSeenTheLastOf >>= traverse_ (sample >=> addToEncounterDeck . only) . nonEmpty

scarletKeyWithEnemy :: ToId enemy EnemyId => enemy -> ScarletKeyMatcher
scarletKeyWithEnemy = ScarletKeyWithEnemy . EnemyWithId . asId

enemyWithScarletKey :: ToId enemy EnemyId => enemy -> Criterion
enemyWithScarletKey = exists . scarletKeyWithEnemy
