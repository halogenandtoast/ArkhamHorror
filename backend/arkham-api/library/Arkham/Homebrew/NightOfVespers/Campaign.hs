module Arkham.Homebrew.NightOfVespers.Campaign (nightOfVespers, NightOfVespers (..), hasPlayed) where

import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.NightOfVespers.CampaignSteps
import Arkham.Homebrew.NightOfVespers.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.NightOfVespers.ChaosBag
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Target (Target (CampaignTarget))
import Arkham.Trait qualified as Trait
import Data.Map.Strict qualified as Map

newtype NightOfVespers = NightOfVespers CampaignAttrs
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightOfVespers :: Difficulty -> NightOfVespers
nightOfVespers = campaign NightOfVespers (CampaignId ":night-of-vespers") "Night of Vespers"

-- Include the current scenario: NextCampaignStep asks nextStep before it adds
-- that scenario to completedSteps. Normalize lead-investigator options too.
hasPlayed :: CampaignAttrs -> CampaignStep -> Bool
hasPlayed attrs step = attrs.normalizedStep == step || any ((== step) . normalizedCampaignStep) attrs.completedSteps

instance IsCampaign NightOfVespers where
  campaignTokens = chaosBagContents
  nextStep (NightOfVespers attrs) = case attrs.normalizedStep of
    PrologueStep -> continue TheEndOfSeasons
    TheEndOfSeasons -> continue TwoMessages
    TwoMessages
      | toCampaignLogKey FinlayODoyleIsAssistingTheInvestigation `member` attrs.log.recorded ->
          continue BehindClosedDoors
      | otherwise -> continue DancingWithDeath
    BehindClosedDoors
      | hasPlayed attrs DancingWithDeath -> continue TheRoseOfTuscany
      | otherwise -> continue DancingWithDeath
    DancingWithDeath
      | hasPlayed attrs BehindClosedDoors -> continue TheRoseOfTuscany
      | otherwise -> continue BehindClosedDoors
    TheRoseOfTuscany -> continue TheTeachingsOfAradia
    TheTeachingsOfAradia -> continue TheThreeMothers
    TheCityOfFog -> afterDestination
    TheGardenOfBones -> afterDestination
    AChorusOfSuffering -> afterDestination
    OurLadiesOfSorrow -> continue MothersMercy
    MothersMercy -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other
   where
    afterDestination
      | all (hasPlayed attrs) [TheCityOfFog, TheGardenOfBones, AChorusOfSuffering] =
          continue OurLadiesOfSorrow
      | otherwise = continue ChooseDestination

{- | Every bag token is sampled without replacement, until a specified symbol
appears or the bag is exhausted. No matching symbol means no award.
-}
researchFavor :: (HasI18n, ReverseQueue m) => CampaignAttrs -> InvestigatorId -> m ()
researchFavor attrs iid = do
  shuffled <- shuffle attrs.chaosBag
  let (before, rest) = break (`elem` [Cultist, Tablet, ElderThing]) shuffled
      revealed = before <> take 1 rest
      award = case headMay rest of
        Just Cultist -> Just Assets.tenebrarumsFavor
        Just Tablet -> Just Assets.suspiriorumsFavor
        Just ElderThing -> Just Assets.lachrymarumsFavor
        _ -> Nothing
  tokens <- traverse (\face -> createChaosToken face <&> revealedByL ?~ iid) revealed
  focusChaosTokens tokens \unfocus -> do
    chooseOneM iid $ unscoped $ labeled "continue" do
      push unfocus
      for_ award $ addCampaignCardToDeck iid DoNotShuffleIn

research :: (HasI18n, ReverseQueue m) => CampaignAttrs -> InvestigatorId -> m ()
research attrs iid = do
  finlay <- getHasRecord FinlayODoyleIsAssistingTheInvestigation
  gigi <- getHasRecord GigiGiulianoIsAssistingTheInvestigation
  let passages =
        [
          ( "police"
          , [Trait.Detective, Trait.Police, Trait.Warden]
          , True
          , YouLearnedAboutTheTenebrarumMurders
          , Just Assets.tenebrarumsFavor
          )
        ,
          ( "armitage"
          , [Trait.Assistant, Trait.Miskatonic, Trait.Scholar, Trait.Wayfarer]
          , True
          , DrArmitageTaughtYouAboutTheThreeMothers
          , Nothing
          )
        ,
          ( "dream"
          , [Trait.Clairvoyant, Trait.Chosen, Trait.Cursed, Trait.Dreamer, Trait.Manifold]
          , True
          , YouHadAPropheticDream
          , Just Assets.lachrymarumsFavor
          )
        ,
          ( "academy"
          , [Trait.Artist, Trait.Performer, Trait.Socialite]
          , True
          , YouLearnedWhereToFindHelenaNussbaum
          , Just Assets.suspiriorumsFavor
          )
        ,
          ( "finlay"
          , [Trait.Criminal, Trait.Drifter, Trait.Hunter]
          , finlay
          , YouDiscoveredAnaGiulianosConnectionToAmaliaVerdi
          , Just Assets.lachrymarumsFavor
          )
        ,
          ( "gigi"
          , [Trait.Criminal, Trait.Drifter, Trait.Hunter]
          , gigi
          , YouDiscoveredAnaGiulianosConnectionToAmaliaVerdi
          , Just Assets.lachrymarumsFavor
          )
        ,
          ( "lodge"
          , [Trait.Civic, Trait.Cultist, Trait.Entrepreneur, Trait.SilverTwilight]
          , True
          , YouWereInvitedToJoinTheTenebrarumLodge
          , Just Assets.tenebrarumsFavor
          )
        ,
          ( "mandel"
          , [Trait.Agency, Trait.Medic, Trait.Reporter, Trait.Veteran]
          , True
          , YouReadSgtMandelsFile
          , Just Assets.suspiriorumsFavor
          )
        ,
          ( "kaslow"
          , [Trait.Believer, Trait.Blessed, Trait.Sorcerer, Trait.Witch]
          , True
          , AnnaKaslowDirectedYouTowardAnOldColleague
          , Nothing
          )
        ]
  eligible <-
    filterM
      ( \(_, traits, enabled, _, _) -> pure enabled <&&> selectAny (InvestigatorWithId iid <> mapOneOf InvestigatorWithTrait traits)
      )
      passages
  chooseOneM iid do
    questionLabeledCard iid
    for_ eligible \(key, _, _, recordKey, favor) -> labeled key do
      storyOnlyBuild [iid] $ setTitle "title" >> p ("passages." <> key)
      record recordKey
      case favor of
        Just card -> addCampaignCardToDeck iid DoNotShuffleIn card
        Nothing -> researchFavor attrs iid

chooseDestination :: (HasI18n, ReverseQueue m) => CampaignAttrs -> Bool -> m ()
chooseDestination attrs guided = do
  let routes =
        [
          ( "theCityOfFog"
          , TheCityOfFog
          , [YouLearnedAboutTheTenebrarumMurders, YouWereInvitedToJoinTheTenebrarumLodge]
          )
        , ("theGardenOfBones", TheGardenOfBones, [YouLearnedWhereToFindHelenaNussbaum, YouReadSgtMandelsFile])
        ,
          ( "aChorusOfSuffering"
          , AChorusOfSuffering
          , [YouHadAPropheticDream, YouDiscoveredAnaGiulianosConnectionToAmaliaVerdi]
          )
        ]
  allLeads <-
    orM
      $ map
        getHasRecord
        [DrArmitageTaughtYouAboutTheThreeMothers, AnnaKaslowDirectedYouTowardAnOldColleague]
  choices <-
    filterM
      ( \(_, step, leads) ->
          pure (not $ hasPlayed attrs step) <&&> (pure (guided || allLeads) <||> orM (map getHasRecord leads))
      )
      routes
  storyWithChooseOneM (setTitle "title" >> p "chooseDestination") do
    for_ choices \(label, step, _) -> labeled label $ push $ NextCampaignStep (continue step)

instance RunMessage NightOfVespers where
  runMessage msg c@(NightOfVespers attrs) = runQueueT $ campaignI18n $ case msg of
    StartCampaign -> do
      scope "intro" $ flavor $ setTitle "title" >> p "body"
      pure c
    CampaignStep PrologueStep -> do
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      scope "additionalRules" do
        flavor $ setTitle "title" >> compose (scope "jinx" $ h3 "title" >> p "body")
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "twoMessages" do
      flavor $ setTitle "title" >> p "intro" >> p "telegram1" >> p "telegram2"
      storyWithChooseOneM (setTitle "title" >> p "decision") do
        labeled "cavalier" do
          flavor $ setTitle "title" >> p "cavalier1" >> p "cavalier2" >> p.basic "finlayAward"
          record FinlayODoyleIsAssistingTheInvestigation
          getInvestigators >>= \iids -> addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.finlayODoyle
          nextCampaignStep
        labeled "parkerHouse" do
          flavor $ setTitle "title" >> p "parkerHouse1" >> p "parkerHouse2" >> p.basic "gigiAward"
          record GigiGiulianoIsAssistingTheInvestigation
          getInvestigators >>= \iids -> addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.gigiGiuliano
          nextCampaignStep
      pure c
    CampaignStep (InterludeStep 2 _) -> scope "theRoseOfTuscany" do
      flavor $ setTitle "title" >> p "intro"
      readers <-
        select
          $ mapOneOf
            InvestigatorWithTitle
            [ "Agatha Crane"
            , "Dexter Drake"
            , "Edith Foster"
            , "Gloria Goldberg"
            , "Kohaku Narukami"
            , "Mandy Thompson"
            , "Daisy Walker"
            , "Harvey Walters"
            ]
      unless (null readers) $ leadChooseOneM do
        questionLabeled "reader"
        targets readers \iid -> do
          storyOnlyBuild [iid] $ setTitle "title" >> p "nameOfTheRose" >> p.basic "bonus"
          recordForInvestigator iid KnowsTheNameOfTheRose
          interludeXp iid $ toBonus "theRoseOfTuscany" 2
      flavor $ setTitle "title" >> p "outro"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 3 _) -> scope "theThreeMothers" do
      flavor $ setTitle "title" >> p "intro" >> p.basic "instructions"
      getInvestigators >>= traverse_ (research attrs)
      push $ CampaignStep $ InterludeStepPart 3 Nothing 2
      pure c
    CampaignStep (InterludeStepPart 3 _ 2) -> scope "theThreeMothers" do
      flavor $ setTitle "title" >> p "outro"
      chooseDestination attrs False
      pure c
    CampaignStep ChooseDestination -> scope "travel" do
      guided <- getHasRecord MaddalenaGuidesYourPath
      unless guided do
        let destinations = mapMaybe (.scenario) attrs.completedSteps
            origin = case headMay destinations of
              Just ":night-of-vespers:136" -> "sanFrancisco"
              Just ":night-of-vespers:168" -> "freiburg"
              _ -> "naples"
        flavor $ setTitle "title" >> p origin
        record MaddalenaGuidesYourPath
      chooseDestination attrs True
      pure c
    CampaignStep (InterludeStep 4 _) -> scope "ourLadiesOfSorrow" do
      flavor $ setTitle "title" >> p "intro"
      finlay <- getHasRecord FinlayODoyleIsAssistingTheInvestigation
      flavor
        $ setTitle "title"
        >> p (if finlay then "finlay" else "gigi")
        >> p.basic (if finlay then "setAsideFinlay" else "setAsideGigi")
      -- Preserve the actual card and owner for Mother's Mercy's Oubliette setup.
      stories <- getCampaignStoryCards
      let codes = if finlay then [":night-of-vespers:032"] else [":night-of-vespers:033", ":night-of-vespers:242"]
      for_ (Map.toList stories) \(iid, cards) -> for_ (filter ((`elem` codes) . toCardCode) cards) \card -> do
        push
          $ InsertGlobal CampaignTarget "nightOfVespers.captive" (object ["owner" .= iid, "card" .= card])
        push $ RemoveCardFromDeckForCampaign iid (toCardId card)
      flavor $ setTitle "title" >> p "outro"
      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> scope "epilogue" do
      survivors <- select $ IncludeEliminated $ not_ KilledInvestigator <> not_ InsaneInvestigator
      unless (null survivors) do
        for_
          [ (TheInvestigatorsDefeatedTheThreeMothersAndEscapedThePalaceOfSorrows, "one")
          , (TheInvestigatorsDefeatedANewbornGoddessAndEscapedThePalaceOfSorrows, "two")
          , (TheInvestigatorsUnboundAFracturedGoddess, "three")
          , (FinlaySurvived, "finlay")
          , (GigiSurvived, "gigi")
          , (ABargainWasMade, "declan")
          , (StolasIsUnbound, "stolas")
          ]
          \(key, passage) ->
            whenM (getHasRecord key) $ flavor $ setTitle "title" >> p passage
        whenM (getHasRecord FinlaySurvived <&&> getHasRecord NaomiOBannionIsAssistingTheInvestigation)
          $ flavor
          $ setTitle "title"
          >> p "naomi"
      gameOverWon
      pure c
    _ -> lift $ defaultCampaignRunner msg c
