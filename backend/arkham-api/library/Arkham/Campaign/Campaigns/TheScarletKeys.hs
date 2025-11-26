module Arkham.Campaign.Campaigns.TheScarletKeys (theScarletKeys) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Types hiding (campaignChaosBag)
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheScarletKeys.CampaignSteps
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta hiding (MapLocationType (..))
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getLead, getLeadPlayer)
import Arkham.Helpers.Xp
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Question
import Arkham.SideStory
import Arkham.Source
import Arkham.Trait (Trait (Drifter))
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map

newtype TheScarletKeys = TheScarletKeys CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theScarletKeys :: Difficulty -> TheScarletKeys
theScarletKeys = campaign TheScarletKeys (CampaignId "09") "The Scarlet Keys"

{- FOURMOLU_DISABLE -}
campaignChaosBag :: Difficulty -> [ChaosTokenFace]
campaignChaosBag = \case
  Easy ->
    [ #"+1", #"+1", #"0", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Standard ->
    [ #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-4"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Hard ->
    [ #"0", #"0", #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4", #"-5"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Expert ->
    [ #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4", #"-4", #"-5", #"-6", #"-8"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
{- FOURMOLU_ENABLE -}

pickSideStory :: ReverseQueue m => CampaignAttrs -> m ()
pickSideStory attrs =
  push
    $ NextCampaignStep
    $ Just
    $ ContinueCampaignStep
    $ Continuation (embark attrs) False True Nothing

travel :: ReverseQueue m => CampaignAttrs -> MapLocationId -> Bool -> Int -> m TheScarletKeys
travel attrs locId doTravel n = do
  markTime n
  let
    attrs' =
      attrs
        & overMeta
          ( (visitedLocationsL %~ if doTravel then (locId :) else id)
              . (currentLocationL .~ locId)
              . (unlockedLocationsL %~ if doTravel then filter (/= locId) else id)
          )
  if doTravel
    then case locId of
      Moscow -> campaignStep_ (InterludeStep 26 Nothing)
      Marrakesh -> campaignStep_ DeadHeat
      BuenosAires -> campaignStep_ SanguineShadows
      Bermuda -> campaignStep_ (InterludeStep 20 Nothing)
      SanFrancisco -> campaignStep_ (InterludeStep 26 Nothing)
      Constantinople -> campaignStep_ DealingsInTheDark
      Anchorage -> campaignStep_ OnThinIce
      Havana -> campaignStep_ DancingMad
      Kabul -> campaignStep_ (InterludeStep 14 Nothing)
      Quito -> campaignStep_ (InterludeStep 14 Nothing)
      SanJuan -> campaignStep_ (InterludeStep 14 Nothing)
      Reykjavik -> campaignStep_ (InterludeStep 14 Nothing)
      London -> campaignStep_ (InterludeStep 27 Nothing)
      Shanghai -> campaignStep_ (InterludeStep 32 Nothing)
      Bombay -> campaignStep_ (InterludeStep 36 Nothing)
      Stockholm -> campaignStep_ (InterludeStep 36 Nothing)
      Lagos -> campaignStep_ (InterludeStep 37 Nothing)
      Tokyo -> campaignStep_ (InterludeStep 37 Nothing)
      RioDeJaneiro -> campaignStep_ (InterludeStep 44 Nothing)
      Manokwari -> campaignStep_ (InterludeStep 45 Nothing)
      Sydney -> campaignStep_ (InterludeStep 49 Nothing)
      HongKong -> campaignStep_ (InterludeStep 50 Nothing)
      Rome -> campaignStep_ (InterludeStep 51 Nothing)
      YborCity -> campaignStep_ (InterludeStep 52 Nothing)
      Kathmandu -> campaignStep_ (InterludeStep 53 Nothing)
      Nairobi -> campaignStep_ (InterludeStep 54 Nothing)
      Perth -> campaignStep_ (InterludeStep 55 Nothing)
      -- side story locations
      Venice -> pickSideStory attrs'
      Cairo -> pickSideStory attrs'
      MonteCarlo -> pickSideStory attrs'
      Arkham -> pickSideStory attrs'
      NewOrleans -> pickSideStory attrs'
      _ -> pure ()
    else campaignStep_ (embark attrs')
  pure $ TheScarletKeys attrs'

instance IsCampaign TheScarletKeys where
  campaignTokens = campaignChaosBag
  invalidCards c =
    "02310" -- The Red-Gloved Man can not be included
      : recordedCardCodes
        ( findWithDefault [] (toCampaignLogKey ErasedFromExistence)
            $ attr (campaignLogRecordedSets . campaignLog) c
        )
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue RiddlesAndRain
    RiddlesAndRain -> continue (InterludeStep 1 Nothing)
    InterludeStep 1 _ -> continue (embark a)
    DeadHeat -> continue (embark a)
    SanguineShadows -> continue (embark a)
    DealingsInTheDark -> continue (embark a)
    DancingMad -> continue (embark a)
    EpilogueStep -> Nothing
    other -> defaultNextStep other

embark :: (Entity a, EntityAttrs a ~ CampaignAttrs) => a -> CampaignStep
embark a = CampaignSpecificStep "embark" currentLocation
 where
  meta = toResult @TheScarletKeysMeta (toAttrs a).meta
  currentLocation = Just $ tshow meta.currentLocation

instance RunMessage TheScarletKeys where
  runMessage msg c@(TheScarletKeys attrs) = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> scope "prologue" do
      flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure $ TheScarletKeys $ attrs & metaL .~ toJSON initMeta
    CampaignStep (InterludeStep 1 _) -> scope "interlude1" do
      storyWithChooseOneM' (setTitle "title" >> p "theFoundation1") do
        labeled' "tell" $ interludeStepPart 1 Nothing 2
        labeled' "doNotTell" $ interludeStepPart 1 Nothing 3
      pure c
    CampaignStep (InterludeStepPart 1 _ 2) -> scope "interlude1" do
      swapTokens ElderThing Tablet
      record TheCellToldTheTruthToTaylor
      flavor $ setTitle "title" >> p "theFoundation2"
      interludeStepPart 1 Nothing 4
      pure c
    CampaignStep (InterludeStepPart 1 _ 3) -> scope "interlude1" do
      swapTokens Tablet ElderThing
      record TheCellHidTheTruthFromTaylor
      flavor $ setTitle "title" >> p "theFoundation3"
      interludeStepPart 1 Nothing 4
      pure c
    CampaignStep (InterludeStepPart 1 _ 4) -> scope "interlude1" do
      flavor $ setTitle "title" >> p "theFoundation4"
      unscoped $ campaignI18n do
        scope "embarkingAndTravel" $ flavor $ setTitle "title" >> p "body"
        scope "theFoundationDossiers" $ flavor $ setTitle "title" >> p "body"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (CampaignSpecificStep "embark" _) -> scope "embark" do
      lead <- getLeadPlayer
      let meta = toResult attrs.meta

      let
        mapKey :: MapLocationId -> Key
        mapKey = Key.fromText . tshow
        toEntry :: MapLocationId -> Pair
        toEntry locId = mapKey locId .= object ["travel" .= mapDistance meta locId]

        hasTicket =
          any (any ((== Assets.expeditedTicket.cardCode) . toCardCode)) (Map.elems attrs.storyCards)

      push
        $ Ask lead
        $ PickCampaignSpecific "embark"
        $ object
          [ "current" .= meta.currentLocation
          , "available" .= meta.unlockedLocations
          , "locations" .= map toEntry [minBound ..]
          , "hasTicket" .= hasTicket
          ]
      pure c
    CampaignSpecific "travel" v -> do
      let locId = toResult v
      let meta = toResult attrs.meta
      let n = fromMaybe 0 (mapDistance meta locId) + (if locId `elem` greenLocations then 1 else 0)
      travel attrs locId True n
    CampaignSpecific "travelVia" v -> do
      let locId = toResult v
      let meta = toResult attrs.meta
      let n = fromMaybe 0 (mapDistance meta locId) + (if locId `elem` greenLocations then 1 else 0)
      travel attrs locId False n
    CampaignSpecific "travelWithTicket" v -> do
      let locId = toResult v
      removeCampaignCard Assets.expeditedTicket
      travel attrs locId True 1
    CampaignSpecific "unlock" v -> do
      let locId = toResult v
      pure
        $ TheScarletKeys
        $ attrs
        & overMeta (unlockedLocationsL %~ (locId :))
    CampaignSpecific "setBearer" v -> do
      let (cardCode, status) = toResult v
      pure $ TheScarletKeys $ attrs & overMeta (keyStatusL %~ insertMap cardCode status)
    CampaignSpecific "desidarioVersion" v -> do
      pure $ updateAttrs c (storeL . at "desidarioVersion" ?~ v)
    CampaignSpecific "unlockedTheta" v -> do
      pure
        $ TheScarletKeys
        $ attrs
        & overMeta (thetaL ?~ toResult v)
    CampaignSpecific "statusReport" v -> scope "statusReport" do
      let statusReport = toResult v
      case statusReport of
        Alpha -> do
          flavor $ setTitle "title" >> p "alpha"
          addChaosToken Cultist
        Beta -> do
          flavor $ setTitle "title" >> p "beta"
          addChaosToken Cultist
          campaignSpecific "unlock" Tunguska
        Epsilon -> flavor $ setTitle "title" >> p "epsilon"
        Zeta -> do
          let meta = toResult @TheScarletKeysMeta attrs.meta
          let
            skeys =
              meta.keyStatus & Map.assocs & mapMaybe \case
                (k, KeyWithInvestigator iid) -> (,iid) <$> lookupCardDef k
                _ -> Nothing
          runMaybeT_ do
            ks <- hoistMaybe $ nonEmpty skeys
            stolen <- lift $ sampleN (if length ks >= 5 then 2 else 1) ks
            culprits <- MaybeT $ nonEmpty <$> haven'tSeenTheLastOf
            lift $ sample culprits >>= \e -> for_ stolen \(k, iid) -> setBearer k (keyStolenByEnemy iid e)

          flavor $ setTitle "title" >> p "zeta"
          campaignSpecific "unlock" Kabul
          campaignSpecific "unlock" Quito
          campaignSpecific "unlock" SanJuan
          campaignSpecific "unlock" Reykjavik
        Gamma -> do
          flavor $ setTitle "title" >> p "gamma"
          addChaosToken Cultist
          eachInvestigator \iid -> addCampaignCardToDeck iid ShuffleIn Treacheries.paradimensionalUnderstanding
        Theta -> flavor $ setTitle "title" >> p "theta"
        Psi -> do
          flavor $ setTitle "title" >> p "psi"
          campaignSpecific "unlock" HongKong
        Omega -> do
          flavor $ setTitle "title" >> p "omega"
          addChaosToken Cultist
      -- handle via Embark
      pure c
    CampaignStep (InterludeStep 14 _) -> scope "rusesAndReclamation" do
      wrongLeads <- getRecordCount WrongLeads
      if wrongLeads == 3
        then doStep 2 msg
        else do
          found <- sample $ True :| replicate (3 - wrongLeads) False
          doStep (if found then 2 else 1) msg
      pure c
    DoStep 1 (CampaignStep (InterludeStep 14 _)) -> scope "rusesAndReclamation" do
      incrementRecordCount WrongLeads 1
      flavor $ setTitle "title" >> p "rusesAndReclamation1"
      campaignStep_ (embark attrs)
      pure c
    DoStep 2 (CampaignStep (InterludeStep 14 _)) -> scope "rusesAndReclamation" do
      flavor $ setTitle "title" >> p "rusesAndReclamation2"
      wrongLeads <- getRecordCount WrongLeads
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let
        stolen =
          meta.keyStatus & Map.assocs & mapMaybe \case
            (k, KeyWithEnemy e (Just iid)) -> (,e,iid) <$> lookupCardDef k
            _ -> Nothing
      for_ (nonEmpty stolen) \((_k, e, _iid) :| _) -> do
        if
          | e == Enemies.theRedGlovedManPurposeUnknown.cardCode ->
              flavor $ setTitle "title" >> p "theRedGlovedMan"
          | e == Enemies.laChicaRojaHotOnYourTrail.cardCode ->
              flavor $ setTitle "title" >> p "laChicaRoja"
          | e == Enemies.theSanguineWatcherHeSeesWhatIsNotThere.cardCode ->
              flavor $ setTitle "title" >> p "theSanguineWatcher"
          | e == Enemies.theBeastInACowlOfCrimsonLeavingATrailOfDestruction.cardCode ->
              flavor $ setTitle "title" >> p "theBeastInACowlOfCrimson"
          | e == Enemies.theClaretKnightHoldsYouInContempt.cardCode ->
              flavor $ setTitle "title" >> p "theClaretKnight"
          | e == Enemies.thorneOpenToNegotiation.cardCode ->
              flavor $ setTitle "title" >> p "thorne"
          | e == Enemies.desiderioDelgadoAlvarezRedInHisLedger.cardCode ->
              flavor $ setTitle "title" >> p "desiderioDelgadoAlvarez"
          | e == Enemies.amaranthScarletScorn.cardCode ->
              flavor $ setTitle "title" >> p "amaranth"
          | e == Enemies.tzuSanNiangAWhisperInYourEar.cardCode ->
              flavor $ setTitle "title" >> p "tzuSanNiang"
          | e == Enemies.alikiZoniUperetriaSpeaksInDeath.cardCode ->
              flavor $ setTitle "title" >> p "alikiZoniUperetria"
          | otherwise -> error "unknown scarlet key enemy"
      for_ stolen \(k, _, iid) -> setBearer k (KeyWithInvestigator iid)
      interludeXpAll (toBonus "bonus" $ 1 + wrongLeads)
      markTime 1
      campaignStep_ (embark attrs)

      let removes = [Kabul, Quito, SanJuan, Reykjavik]
      let visited = removes \\ meta.visitedLocations
      -- removes are the intersection

      let
        attrs' =
          attrs
            & overMeta
              ( (visitedLocationsL <>~ visited)
                  . (unlockedLocationsL %~ filter (`notElem` removes))
              )
      pure $ TheScarletKeys attrs'
    CampaignStep (InterludeStep 20 _) -> scope "theGreatWork" do
      ok <- getHasRecord TuwileMasaiFledToBermuda
      flavor do
        setTitle "title"
        p "theGreatWork1"
        ul do
          li.validate ok "activity"
          li.validate (not ok) "noActivity"
      interludeStepPart 20 Nothing $ if ok then 2 else 5
      pure c
    CampaignStep (InterludeStepPart 20 _ 2) -> scope "theGreatWork" do
      n <-
        countHasRecords
          [ EceDoesNotTrustTheCell
          , YouHaventSeenTheLastOfLaChicaRoja
          , YouHaventSeenTheLastOfDesiderioDelgadoAlvarez
          , YouHaventSeenTheLastOfTheClaretKnight
          , YouHaventSeenTheLastOfThorne
          , YouHaventSeenTheLastOfAlikiZoniUperetria
          ]

      flavor do
        setTitle "title"
        p "theGreatWork2"
        ul do
          li.validate (n >= 2) "proceed"
          li.validate (n < 2) "otherwise"
      interludeStepPart 20 Nothing $ if n >= 2 then 3 else 4
      pure c
    CampaignStep (InterludeStepPart 20 _ 3) -> scope "theGreatWork" do
      interludeXpAll (toBonus "bonus" 3)
      flavor $ setTitle "title" >> p "theGreatWork3"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStepPart 20 _ 4) -> scope "theGreatWork" do
      record TuwileMasaiIsOnYourSide
      interludeXpAll (toBonus "bonus" 1)
      markTime 1
      flavor $ setTitle "title" >> p "theGreatWork4"
      chooseBearer Keys.theBaleEngine
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStepPart 20 _ 5) -> scope "theGreatWork" do
      flavor $ setTitle "title" >> p "theGreatWork5"
      eachInvestigator \iid -> setupModifier CampaignSource iid (StartingHand 1)
      campaignStep_ (embark attrs)
      pure $ TheScarletKeys $ attrs & overMeta (canResetL %~ (Bermuda :))
    CampaignStep (InterludeStep 26 _) -> scope "quidProQuo" do
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let current = meta.currentLocation
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate (current == SanFrancisco) "sanFrancisco"
          li.validate (current == Moscow) "moscow"
      interludeStepPart 26 Nothing $ if current == SanFrancisco then 1 else 2
      pure c
    CampaignStep (InterludeStepPart 26 _ 1) -> scope "quidProQuo" do
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let visitedMarrakesh = Marrakesh `elem` meta.visitedLocations
      storyWithChooseOneM' (setTitle "title" >> p "quidProQuo1") do
        labeled' "ticket" do
          lead <- getLead
          forceAddCampaignCardToDeckChoice [lead] DoNotShuffleIn Assets.expeditedTicket
          campaignStep_ (embark attrs)
        labeled' "supplies" do
          interludeXpAll (toBonus "supplies" 1)
          campaignStep_ (embark attrs)
        labeledValidate' (not visitedMarrakesh) "intel11" $ interludeStepPart 26 Nothing 3
      pure c
    CampaignStep (InterludeStepPart 26 _ 2) -> scope "quidProQuo" do
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let visitedHavana = Havana `elem` meta.visitedLocations
      storyWithChooseOneM' (setTitle "title" >> p "quidProQuo2") do
        labeled' "ticket" do
          lead <- getLead
          forceAddCampaignCardToDeckChoice [lead] DoNotShuffleIn Assets.expeditedTicket
          campaignStep_ (embark attrs)
        labeled' "supplies" do
          interludeXpAll (toBonus "supplies" 1)
          campaignStep_ (embark attrs)
        labeledValidate' (not visitedHavana) "intel28" $ interludeStepPart 26 Nothing 4
      pure c
    CampaignStep (InterludeStepPart 26 _ 3) -> scope "quidProQuo" do
      record TheCellKnowsAmaranthsRealName
      flavor $ setTitle "title" >> p "quidProQuo3"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStepPart 26 _ 4) -> scope "quidProQuo" do
      record TheCellKnowsOfDesisPast
      flavor $ setTitle "title" >> p "quidProQuo4"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 27 _) -> scope "deadAndGone" do
      noTrust <- getHasRecord AgentQuinnDoesNotTrustTheCell
      flavor do
        setTitle "title"
        p "deadAndGone1"
        ul do
          li.validate noTrust "noTrust"
          li.validate (not noTrust) "trust"
      doStep (if noTrust then 2 else 5) msg
      pure c
    DoStep 2 msg'@(CampaignStep (InterludeStep 27 _)) -> scope "deadAndGone" do
      storyWithChooseOneM' (setTitle "title" >> p "deadAndGone2") do
        labeled' "truth" $ doStep 3 msg'
        labeled' "secrets" $ doStep 4 msg'
      pure c
    DoStep 3 msg'@(CampaignStep (InterludeStep 27 _)) -> scope "deadAndGone" do
      crossOut AgentQuinnDoesNotTrustTheCell
      swapTokens ElderThing Tablet
      whistle <- getHasRecord TheCellPossessesAMysteriousWhistle
      t <- getTime
      let goOffMission = whistle && t <= 30
      flavor do
        setTitle "title"
        p "deadAndGone3"
        ul do
          li "gainTrust"
          li "swapElderThingForTablet"
          li.nested "check" do
            li.validate goOffMission "goOffMission"
            li.validate (not goOffMission) "stayOnMission"

      doStep (if goOffMission then 6 else 7) msg'
      pure c
    DoStep 4 (CampaignStep (InterludeStep 27 _)) -> scope "deadAndGone" do
      record TheFoundationRemainsInTheDark
      swapTokens Tablet ElderThing
      flavor $ setTitle "title" >> p "deadAndGone4"
      campaignStep_ (embark attrs)
      pure c
    DoStep 5 msg'@(CampaignStep (InterludeStep 27 _)) -> scope "deadAndGone" do
      flavor $ setTitle "title" >> p "deadAndGone5"
      whistle <- getHasRecord TheCellPossessesAMysteriousWhistle
      t <- getTime
      let goOffMission = whistle && t <= 30
      flavor do
        setTitle "title"
        p "deadAndGone5"
        ul do
          li.validate goOffMission "goOffMission"
          li.validate (not goOffMission) "stayOnMission"
      doStep (if goOffMission then 6 else 7) msg'
      pure c
    DoStep 6 (CampaignStep (InterludeStep 27 _)) -> scope "deadAndGone" do
      record TheCellIsOffMission
      interludeXpAll (toBonus "bonus" 1)
      flavor $ setTitle "title" >> p "deadAndGone6"
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 7 (CampaignStep (InterludeStep 27 _)) -> scope "deadAndGone" do
      interludeXpAll (toBonus "bonus" 2)
      flavor $ setTitle "title" >> p "deadAndGone7"
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 32 _) -> scope "theCoiledSerpent" do
      toldTheTruth <- getHasRecord TheCellToldTheTruthToTaylor
      flavor do
        setTitle "title"
        p "theCoiledSerpent1"
        ul do
          li.validate toldTheTruth "toldTheTruth"
          li.validate (not toldTheTruth) "hidTheTruth"

      doStep (if toldTheTruth then 2 else 3) msg
      pure c
    DoStep 2 (CampaignStep (InterludeStep 32 _)) -> scope "theCoiledSerpent" do
      record FlintIsWorkingSolo
      t <- getTime
      let psi = min 35 (t + 6)
      flavor $ setTitle "title" >> p "theCoiledSerpent2"
      campaignStep_ (embark attrs)
      pure $ TheScarletKeys $ attrs & overMeta (psiL ?~ psi)
    DoStep 3 msg'@(CampaignStep (InterludeStep 32 _)) -> scope "theCoiledSerpent" do
      storyWithChooseOneM' (setTitle "title" >> p "theCoiledSerpent3") do
        labeled' "keepInquiring" $ doStep 4 msg'
        labeled' "stickAround" $ doStep 5 msg'
        labeled' "forgetHer" $ doStep 6 msg'
      pure c
    DoStep 4 (CampaignStep (InterludeStep 32 _)) -> scope "theCoiledSerpent" do
      record FlintIsWorkingSolo
      t <- getTime
      let psi = min 35 (t + 6)
      flavor $ setTitle "title" >> p "theCoiledSerpent4"
      campaignStep_ (embark attrs)
      pure $ TheScarletKeys $ attrs & overMeta (psiL ?~ psi)
    DoStep 5 (CampaignStep (InterludeStep 32 _)) -> scope "theCoiledSerpent" do
      record TheCellAidedInFlintsInvestigation
      interludeXpAll (toBonus "bonus" 1)
      flavor $ setTitle "title" >> p "theCoiledSerpent5"
      campaignSpecific "unlock" HongKong
      markTime 3
      campaignStep_ (embark attrs)
      pure c
    DoStep 6 (CampaignStep (InterludeStep 32 _)) -> scope "theCoiledSerpent" do
      record FlintAbandonedHisSearch
      flavor $ setTitle "title" >> p "theCoiledSerpent6"
      iids <- allInvestigators
      addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.inspectorFlintWithPrideAndCare
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 36 _) -> scope "strangeArchitecture" do
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let inBombay = meta.currentLocation == Bombay
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate inBombay "inBombay"
          li.validate (not inBombay) "inStockholm"
      doStep (if inBombay then 1 else 4) msg
      pure c
    DoStep 1 msg'@(CampaignStep (InterludeStep 36 _)) -> scope "strangeArchitecture" do
      appreciated <- getHasRecord TheCellAppreciatedTheArchitecture
      flavor do
        setTitle "title"
        p "strangeArchitecture1"
        ul $ scope "bombay" do
          li.validate appreciated "appreciated"
          li.validate (not appreciated) "notAppreciated"
      doStep (if appreciated then 2 else 3) msg'
      pure c
    DoStep 2 (CampaignStep (InterludeStep 36 _)) -> scope "strangeArchitecture" do
      crossOut TheCellAppreciatedTheArchitecture
      interludeXpAll (toBonus "bonus" 2)
      flavor $ setTitle "title" >> p "strangeArchitecture2"
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 36 _)) -> scope "strangeArchitecture" do
      record TheCellAppreciatedTheArchitecture
      flavor $ setTitle "title" >> p "strangeArchitecture3"
      campaignStep_ (embark attrs)
      pure c
    DoStep 4 msg'@(CampaignStep (InterludeStep 36 _)) -> scope "strangeArchitecture" do
      appreciated <- getHasRecord TheCellAppreciatedTheArchitecture
      flavor do
        setTitle "title"
        p "strangeArchitecture4"
        ul $ scope "stockholm" do
          li.validate appreciated "appreciated"
          li.validate (not appreciated) "notAppreciated"
      doStep (if appreciated then 5 else 6) msg'
      pure c
    DoStep 5 (CampaignStep (InterludeStep 36 _)) -> scope "strangeArchitecture" do
      crossOut TheCellAppreciatedTheArchitecture
      interludeXpAll (toBonus "bonus" 2)
      flavor $ setTitle "title" >> p "strangeArchitecture5"
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 6 (CampaignStep (InterludeStep 36 _)) -> scope "strangeArchitecture" do
      record TheCellAppreciatedTheArchitecture
      flavor $ setTitle "title" >> p "strangeArchitecture6"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 37 _) -> scope "specialDelivery" do
      deliveringIntel <- getHasRecord TheCellIsDeliveringFoundationIntel
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let inTokyo = meta.currentLocation == Tokyo
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.nested.validate deliveringIntel "deliveringIntel" do
            li.validate (deliveringIntel && inTokyo) "deliveringIntelInTokyo"
            li.validate (deliveringIntel && not inTokyo) "deliveringIntelInLagos"
          li.nested.validate (not deliveringIntel) "notDeliveringIntel" do
            li.validate (not deliveringIntel && inTokyo) "notDeliveringIntelInTokyo"
            li.validate (not deliveringIntel && not inTokyo) "notDeliveringIntelInLagos"
      if
        | deliveringIntel && inTokyo -> doStep 2 msg
        | deliveringIntel && not inTokyo -> doStep 4 msg
        | not deliveringIntel && inTokyo -> doStep 1 msg
        | otherwise -> doStep 3 msg
      pure c
    DoStep 1 (CampaignStep (InterludeStep 37 _)) -> scope "specialDelivery" do
      record TheCellIsDeliveringFoundationIntel
      flavor $ setTitle "title" >> p "specialDelivery1"
      eachInvestigator \iid -> forceAddCampaignCardToDeckChoice [iid] DoNotShuffleIn Assets.foundationIntel
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 2 (CampaignStep (InterludeStep 37 _)) -> scope "specialDelivery" do
      crossOut TheCellIsDeliveringFoundationIntel
      flavor $ setTitle "title" >> p "specialDelivery2"
      let faces = filter isNumberChaosToken $ nub attrs.chaosBag
      leadChooseOneM do
        questionLabeled' "chooseToken"
        for_ faces \face ->
          when (face /= PlusOne) do
            chaosTokenLabeled face do
              removeChaosToken face
              case face of
                Zero -> addChaosToken PlusOne
                MinusOne -> addChaosToken Zero
                MinusTwo -> addChaosToken MinusOne
                MinusThree -> addChaosToken MinusTwo
                MinusFour -> addChaosToken MinusThree
                MinusFive -> addChaosToken MinusFour
                MinusSix -> addChaosToken MinusFive
                MinusSeven -> addChaosToken MinusSix
                MinusEight -> addChaosToken MinusSeven
                _ -> pure ()
      eachInvestigator (`removeCampaignCardFromDeck` Assets.foundationIntel)
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 37 _)) -> scope "specialDelivery" do
      record TheCellIsDeliveringFoundationIntel
      flavor $ setTitle "title" >> p "specialDelivery3"
      eachInvestigator \iid -> forceAddCampaignCardToDeckChoice [iid] DoNotShuffleIn Assets.foundationIntel
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 4 (CampaignStep (InterludeStep 37 _)) -> scope "specialDelivery" do
      crossOut TheCellIsDeliveringFoundationIntel
      flavor $ setTitle "title" >> p "specialDelivery4"
      let faces = filter isNumberChaosToken $ nub attrs.chaosBag
      leadChooseOneM do
        questionLabeled' "chooseToken"
        for_ faces \face ->
          when (face /= PlusOne) do
            chaosTokenLabeled face do
              removeChaosToken face
              case face of
                Zero -> addChaosToken PlusOne
                MinusOne -> addChaosToken Zero
                MinusTwo -> addChaosToken MinusOne
                MinusThree -> addChaosToken MinusTwo
                MinusFour -> addChaosToken MinusThree
                MinusFive -> addChaosToken MinusFour
                MinusSix -> addChaosToken MinusFive
                MinusSeven -> addChaosToken MinusSix
                MinusEight -> addChaosToken MinusSeven
                _ -> pure ()
      eachInvestigator (`removeCampaignCardFromDeck` Assets.foundationIntel)
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 44 _) -> scope "theoryOfAnnihilation" do
      t <- getTime
      met <- getHasRecord TheCellMetDrIrawan
      flavor do
        setTitle "title"
        p "theoryOfAnnihilation1"
        ul do
          li.validate (met && t < 25) "traveled"
          li.validate (met && t >= 25) "vanished"
          li.validate (not met) "met"
      if
        | met && t < 25 -> doStep 2 msg
        | met && t >= 25 -> doStep 3 msg
        | otherwise -> doStep 4 msg
      pure c
    DoStep 2 (CampaignStep (InterludeStep 44 _)) -> scope "theoryOfAnnihilation" do
      record DrIrawanTraveledToNewGuinea
      interludeXpAll (toBonus "bonus" 1)
      campaignSpecific "unlock" Manokwari
      flavor $ setTitle "title" >> p "theoryOfAnnihilation2"
      campaignStep_ (embark attrs)
      t <- getTime
      pure
        $ TheScarletKeys
        $ attrs
        & overMeta (deltaL ?~ t)
    DoStep 3 (CampaignStep (InterludeStep 44 _)) -> scope "theoryOfAnnihilation" do
      record DrIrawanVanishedFromExistence
      flavor $ setTitle "title" >> p "theoryOfAnnihilation3"
      campaignStep_ (embark attrs)
      pure c
    DoStep 4 (CampaignStep (InterludeStep 44 _)) -> scope "theoryOfAnnihilation" do
      record TheCellMetDrIrawan
      interludeXpAll (toBonus "bonus" 1)
      flavor $ setTitle "title" >> p "theoryOfAnnihilation4"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 45 _) -> scope "metamorphosis" do
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let delta = fromMaybe (error "Missing delta") meta.delta
      t <- getTime
      flavor do
        setTitle "title"
        p "metamorphosis1"
        ul do
          li.validate (t - delta <= 10) "tenOrLessTime"
          li.validate (t - delta > 10) "moreThanTenTime"
      doStep (if t - delta <= 10 then 2 else 3) msg
      pure c
    DoStep 2 (CampaignStep (InterludeStep 45 _)) -> scope "metamorphosis" do
      flavor $ setTitle "title" >> p "metamorphosis2"
      iids <- allInvestigators
      addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.drDewiIrawanCryptozoologist
      chooseBearer Keys.theRuinousChime
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 45 _)) -> scope "metamorphosis" do
      record DrIrawanVanishedFromExistence
      interludeXpAll (toBonus "bonus" 2)
      flavor $ setTitle "title" >> p "metamorphosis3"
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 49 _) -> scope "ringingHollow" do
      t <- getTime
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate (t < 20) "lessThanTwentyTime"
          li.validate (t >= 20) "twentyOrMoreTime"
      doStep (if t < 20 then 1 else 4) msg
      pure c
    DoStep 1 msg'@(CampaignStep (InterludeStep 49 _)) -> scope "ringingHollow" do
      toldTheTruth <- getHasRecord TheCellToldTheTruthToTaylor
      flavor do
        setTitle "title"
        p "ringingHollow1"
        ul do
          li.validate toldTheTruth "toldTheTruth"
          li.validate (not toldTheTruth) "hidTheTruth"

      doStep (if toldTheTruth then 2 else 3) msg'
      pure c
    DoStep 2 (CampaignStep (InterludeStep 49 _)) -> scope "ringingHollow" do
      flavor $ setTitle "title" >> p "ringingHollow2"
      campaignSpecific "unlock" London
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 49 _)) -> scope "ringingHollow" do
      record AgentQuinnDoesNotTrustTheCell
      flavor $ setTitle "title" >> p "ringingHollow3"
      campaignSpecific "unlock" London
      campaignStep_ (embark attrs)
      pure c
    DoStep 4 (CampaignStep (InterludeStep 49 _)) -> scope "ringingHollow" do
      record AgentQuinnVanishedFromExistence
      flavor $ setTitle "title" >> p "ringingHollow4"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 50 _) -> scope "bloodSweatAndTea" do
      t <- getTime
      aided <- getHasRecord TheCellAidedInFlintsInvestigation
      solo <- getHasRecord FlintIsWorkingSolo
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let psi = fromMaybe (error "Missing psi") meta.psi

      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate aided "aided"
          li.validate (solo && t - psi <= 10) "tenOrLessTime"
          li.validate (solo && t - psi > 10) "moreThanTenTime"
      if
        | aided -> doStep 1 msg
        | solo && t - psi <= 10 -> doStep 2 msg
        | otherwise -> doStep 3 msg
      pure c
    DoStep 1 (CampaignStep (InterludeStep 50 _)) -> scope "bloodSweatAndTea" do
      record FlintTraveledToKualaLumpur
      flavor $ setTitle "title" >> p "bloodSweatAndTea1"
      campaignSpecific "unlock" KualaLumpur
      campaignStep_ (embark attrs)
      pure c
    DoStep 2 (CampaignStep (InterludeStep 50 _)) -> scope "bloodSweatAndTea" do
      record FlintTraveledToKualaLumpur
      flavor $ setTitle "title" >> p "bloodSweatAndTea2"
      campaignSpecific "unlock" KualaLumpur
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 50 _)) -> scope "bloodSweatAndTea" do
      record AgentFlintIsMissing
      flavor $ setTitle "title" >> p "bloodSweatAndTea3"
      campaignSpecific "unlock" KualaLumpur
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 51 _) -> scope "romulusAndRemus" do
      offMission <- getHasRecord TheCellIsOffMission

      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate offMission "offMission"
          li.validate (not offMission) "onMission"
      doStep (if offMission then 1 else 2) msg
      pure c
    DoStep 1 (CampaignStep (InterludeStep 51 _)) -> scope "romulusAndRemus" do
      flavor $ setTitle "title" >> p "romulusAndRemus1"
      campaignSpecific "unlock" BermudaTriangle
      campaignStep_ (embark attrs)
      pure c
    DoStep 2 (CampaignStep (InterludeStep 51 _)) -> scope "romulusAndRemus" do
      flavor $ setTitle "title" >> p "romulusAndRemus2"
      eachInvestigator \iid -> setupModifier CampaignSource iid (StartingResources 1)
      campaignStep_ (embark attrs)
      pure $ TheScarletKeys $ attrs & overMeta (canResetL %~ (Rome :))
    CampaignStep (InterludeStep 52 _) -> scope "theSafehouse" do
      t <- getTime
      knowThePassphrase <- getHasRecord YouKnowThePassphrase
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let atOrAfterTheta = maybe False (t >=) meta.theta
      let safehouse = knowThePassphrase && atOrAfterTheta

      flavor do
        setTitle "title"
        p "theSafehouse1"
        ul do
          li.validate safehouse "safehouse"
          li.validate (not safehouse) "noSafehouse"
      doStep (if safehouse then 2 else 3) msg
      pure c
    DoStep 2 (CampaignStep (InterludeStep 52 _)) -> scope "theSafehouse" do
      interludeXpAll (toBonus "bonus" 2)
      flavor $ setTitle "title" >> p "theSafehouse2"
      chooseBearer Keys.theMirroringBlade
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 52 _)) -> scope "theSafehouse" do
      flavor $ setTitle "title" >> p "theSafehouse3"
      campaignStep_ (embark attrs)
      pure $ TheScarletKeys $ attrs & overMeta (canResetL %~ (YborCity :))
    CampaignStep (InterludeStep 53 _) -> scope "whistleOnTheWind" do
      drifter <-
        selectAny
          $ InvestigatorWithTrait Drifter
          <> not_ (mapOneOf investigatorIs [Investigators.wendyAdams, Investigators.wendyAdamsParallel])
      storyWithChooseOneM'
        ( setTitle "title"
            >> p "whistleOnTheWind1Part1"
            >> p.validate drifter "drifter"
            >> p "whistleOnTheWind1Part2"
        )
        do
          labeled' "accept" $ doStep 2 msg
          labeled' "reject" $ doStep 3 msg
      pure c
    DoStep 2 (CampaignStep (InterludeStep 53 _)) -> scope "whistleOnTheWind" do
      record TheCellPossessesAMysteriousWhistle
      swapTokens ElderThing Tablet
      flavor $ setTitle "title" >> p "whistleOnTheWind2"
      eachInvestigator \iid -> chooseOneM iid $ unscoped do
        questionLabeled' "chooseTrauma"
        countVar 1 $ labeled' "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
        countVar 1 $ labeled' "sufferMentalTrauma" $ sufferMentalTrauma iid 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 53 _)) -> scope "whistleOnTheWind" do
      record TheCellRefusedAlikisOffer
      interludeXpAll (toBonus "bonus" 1)
      swapTokens Tablet ElderThing
      flavor $ setTitle "title" >> p "whistleOnTheWind3"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 54 _) -> scope "infernalMachinery" do
      n <-
        countHasRecords
          [ LaChicaRojaIsOnYourSide
          , TheCellAidedTheKnight
          , AlikiIsOnYourSide
          , DesiIsInYourDebt
          , TheCellMadeADealWithThorne
          , EceTrustsTheCell
          ]
      flavor do
        setTitle "title"
        p "infernalMachinery1"
        ul do
          li.validate (n >= 3) "reputation"
          li.validate (n < 3) "noReputation"
      doStep (if n >= 3 then 2 else 3) msg
      pure c
    DoStep 2 (CampaignStep (InterludeStep 54 _)) -> scope "infernalMachinery" do
      record TuwileMasaiIsOnYourSide
      interludeXpAll (toBonus "bonus" 1)
      flavor $ setTitle "title" >> p "infernalMachinery2"
      chooseBearer Keys.theBaleEngine
      markTime 1
      campaignStep_ (embark attrs)
      pure c
    DoStep 3 (CampaignStep (InterludeStep 54 _)) -> scope "infernalMachinery" do
      record TuwileMasaiFledToBermuda
      flavor $ setTitle "title" >> p "infernalMachinery3"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (InterludeStep 55 _) -> scope "paranaturalSelection" do
      t <- getTime
      met <- getHasRecord TheCellMetDrIrawan
      flavor do
        setTitle "title"
        p "paranaturalSelection1"
        ul do
          li.validate (met && t < 25) "traveled"
          li.validate (met && t >= 25) "vanished"
          li.validate (not met) "met"
      if
        | met && t < 25 -> doStep 2 msg
        | met && t >= 25 -> doStep 3 msg
        | otherwise -> doStep 4 msg
      pure c
    DoStep 2 (CampaignStep (InterludeStep 55 _)) -> scope "paranaturalSelection" do
      record DrIrawanTraveledToNewGuinea
      interludeXpAll (toBonus "bonus" 1)
      campaignSpecific "unlock" Manokwari
      flavor $ setTitle "title" >> p "paranaturalSelection2"
      campaignStep_ (embark attrs)
      t <- getTime
      pure
        $ TheScarletKeys
        $ attrs
        & overMeta (deltaL ?~ t)
    DoStep 3 (CampaignStep (InterludeStep 55 _)) -> scope "paranaturalSelection" do
      record DrIrawanVanishedFromExistence
      flavor $ setTitle "title" >> p "paranaturalSelection3"
      campaignStep_ (embark attrs)
      pure c
    DoStep 4 (CampaignStep (InterludeStep 55 _)) -> scope "paranaturalSelection" do
      record TheCellMetDrIrawan
      interludeXpAll (toBonus "bonus" 1)
      flavor $ setTitle "title" >> p "paranaturalSelection4"
      campaignStep_ (embark attrs)
      pure c
    CampaignStep (ScenarioStep _) -> do
      TheScarletKeys attrs' <- lift $ defaultCampaignRunner msg c
      let meta = toResult @TheScarletKeysMeta attrs'.meta
      pure
        $ TheScarletKeys
        $ attrs
        & overMeta ((canResetL .~ []) . (unlockedLocationsL %~ (meta.canReset <>)))
    CampaignStep (StandaloneScenarioStep sid _) -> do
      markTime $ getSideStoryCost sid
      pushAll [ResetInvestigators, ResetGame, StartScenario sid Nothing]
      pure c
    _ -> lift $ defaultCampaignRunner msg c
