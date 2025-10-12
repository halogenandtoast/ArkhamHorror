module Arkham.Campaign.Campaigns.TheScarletKeys (theScarletKeys) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.CampaignSteps
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Meta hiding (MapLocationType (..))
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead, getLeadPlayer)
import Arkham.Helpers.Xp
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Question
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

travel :: ReverseQueue m => CampaignAttrs -> MapLocationId -> Bool -> Int -> m TheScarletKeys
travel attrs locId doTravel n = do 
  markTime n
  if doTravel
    then case locId of
      Moscow -> campaignStep_ (InterludeStep 26 Nothing)
      _ -> pure ()
    else campaignStep_ (CampaignSpecificStep "embark")
  pure
    $ TheScarletKeys
    $ attrs
    & overMeta
      ( (visitedLocationsL %~ (locId :))
          . (currentLocationL .~ locId)
          . (unlockedLocationsL %~ filter (/= locId))
      )

instance IsCampaign TheScarletKeys where
  campaignTokens = campaignChaosBag
  invalidCards _ = ["02310"] -- The Red-Gloved Man can not be included
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just RiddlesAndRain
    RiddlesAndRain -> Just (InterludeStep 1 Nothing)
    InterludeStep 1 _ -> Just (CampaignSpecificStep "embark")
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

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
      flavor $ setTitle "title" >> p "theFoundation2"
      interludeStepPart 1 Nothing 4
      pure c
    CampaignStep (InterludeStepPart 1 _ 3) -> scope "interlude1" do
      flavor $ setTitle "title" >> p "theFoundation3"
      interludeStepPart 1 Nothing 4
      pure c
    CampaignStep (InterludeStepPart 1 _ 4) -> scope "interlude1" do
      flavor $ setTitle "title" >> p "theFoundation4"
      unscoped $ campaignI18n do
        scope "embarkingAndTravel" $ flavor $ setTitle "title" >> p "body"
        scope "theFoundationDossiers" $ flavor $ setTitle "title" >> p "body"
      campaignStep_ (CampaignSpecificStep "embark")
      pure c
    CampaignStep (CampaignSpecificStep "embark") -> scope "embark" do
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
    CampaignSpecific "setBearer" v -> do
      let (cardCode, status) = toResult v
      pure $ TheScarletKeys $ attrs & overMeta (keyStatusL %~ insertMap cardCode status)
    CampaignStep (InterludeStep 26 _) -> scope "quidProQuo" do
      -- Moscow
      let meta = toResult @TheScarletKeysMeta attrs.meta
      let visitedHavana = Havana `elem` meta.visitedLocations
      storyWithChooseOneM' (setTitle "title" >> p "body" >> p "quidProQuo2") do
        labeled' "ticket" do
          lead <- getLead
          forceAddCampaignCardToDeckChoice [lead] DoNotShuffleIn Assets.expeditedTicket
          campaignStep_ (CampaignSpecificStep "embark")
        labeled' "supplies" do
          interludeXpAll (toBonus "supplies" 1)
          campaignStep_ (CampaignSpecificStep "embark")
        labeledValidate' (not visitedHavana) "information" $ interludeStepPart 26 Nothing 4
      pure c
    CampaignStep (InterludeStepPart 26 _ 4) -> scope "quidProQuo" do
      record TheCellKnowsOfDesisPast
      flavor $ setTitle "title" >> p "quidProQuo4"
      campaignStep_ (CampaignSpecificStep "embark")
      pure c
    _ -> lift $ defaultCampaignRunner msg c
