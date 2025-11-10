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
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead, getLeadPlayer)
import Arkham.Helpers.Xp
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Question
import Arkham.SideStory
import Arkham.Source
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
pickSideStory attrs = push $ NextCampaignStep $ Just $ ContinueCampaignStep $ Continuation (embark attrs) False True

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
      Havana -> campaignStep_ DancingMad
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
      flavor $ setTitle "title" >> p "theFoundation2"
      interludeStepPart 1 Nothing 4
      pure c
    CampaignStep (InterludeStepPart 1 _ 3) -> scope "interlude1" do
      swapTokens Tablet ElderThing
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
          , YouHaventSeenTheLastOfThorn
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
    CampaignStep (ScenarioStep _) -> do
      TheScarletKeys attrs' <- lift $ defaultCampaignRunner msg c
      let meta = toResult @TheScarletKeysMeta attrs'.meta
      pure
        $ TheScarletKeys
        $ attrs
        & overMeta ((canResetL .~ []) . (unlockedLocationsL %~ (meta.canReset <>)))
    CampaignStep (StandaloneScenarioStep sid _) -> do
      markTime $ getSideStoryCost sid
      pushAll [ResetInvestigators, ResetGame, StartScenario sid]
      pure c
    _ -> lift $ defaultCampaignRunner msg c
