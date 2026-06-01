module Arkham.Scenario.Scenarios.PreludeDawnOfTheFinalDay (preludeDawnOfTheFinalDay) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.I18n
import Arkham.Id (PlayerId, getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Import.Lifted
import Arkham.Story.Cards qualified as Stories

newtype PreludeDawnOfTheFinalDay = PreludeDawnOfTheFinalDay ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preludeDawnOfTheFinalDay :: Difficulty -> PreludeDawnOfTheFinalDay
preludeDawnOfTheFinalDay difficulty =
  scenarioWith
    PreludeDawnOfTheFinalDay
    "10679a"
    "The Vale"
    difficulty
    [ ".     triangle square"
    , "moon  triangle square"
    , "moon  diamond  star"
    , "heart diamond  star"
    , "heart circle   spade"
    , ".     circle   spade"
    ]
    $ (hasEncounterDeckL .~ False)
    . (referenceL .~ "10704")

instance HasChaosTokenValue PreludeDawnOfTheFinalDay where
  getChaosTokenValue iid tokenFace (PreludeDawnOfTheFinalDay attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PreludeDawnOfTheFinalDay where
  runMessage msg s@(PreludeDawnOfTheFinalDay attrs) = runQueueT $ campaignI18n $ scope "prelude3" $ case msg of
    PreScenarioSetup -> scope "intro" do
      hasPlan <- getHasRecord DrMarquezHasAPlan
      -- Check your Campaign Log. If Dr. Marquez has a plan, proceed to Intro 1,
      -- otherwise skip to Intro 2.
      flavor do
        h "title"
        p "checkCampaignLog"
        ul do
          li.validate hasPlan "proceedToIntro1"
          li.validate (not hasPlan) "skipToIntro2"
      if hasPlan
        then flavor $ h "title" >> p "intro1"
        else flavor $ h "title" >> p "intro2"
      flavor $ h "title" >> p "intro3"
      -- More survey assistance has arrived. Any player whose investigator was
      -- killed in The Longest Night may select a new investigator and upgrade
      -- their new deck to half the earned experience of their previous
      -- investigator (rounded up). The campaign transition into this prelude is
      -- set up to defer the killed/insane handling to this point (see
      -- Arkham.Campaign.Campaigns.TheFeastOfHemlockVale), so we trigger the
      -- new-investigator selection here and then award the half experience.
      killed <- select KilledInvestigator
      halfXp <- for killed \iid -> do
        pid <- getPlayer iid
        xp <- field InvestigatorXp iid
        pure (pid, (xp + 1) `div` 2)
      unless (null halfXp) do
        push $ Msg.chooseUpgradeDecks (map fst halfXp)
        doStep 1 (ScenarioSpecific "survivorXp" (toJSON halfXp))
      -- The fatigue from the long night catches up to you.
      replaceFatigueChaosTokens
      pure s
    DoStep 1 (ScenarioSpecific "survivorXp" v) -> do
      let halfXp = toResult v :: [(PlayerId, Int)]
      for_ halfXp \(pid, n) -> when (n > 0) do
        selectOne (InvestigatorIsPlayer pid) >>= traverse_ \iid ->
          gainXp iid ScenarioSource (ikey "xp.survivor") n
      pure s
    Setup -> runScenarioSetup PreludeDawnOfTheFinalDay attrs do
      setup $ ul do
        li "gatherSets"
        li "dayThree"
        li "buildActAgenda"
        li.nested "placeLocations" do
          li "crossroadsAndOldMill"
          li "startAt"
        li.nested "residents" do
          li "removeResidents"
          li "motherRachelAndJudith"
          li "setOutOfPlay"
        li "setAsideAgents"
        li "placeDoom"
        unscoped $ li "readyToBegin"

      gather Set.TheFinalDay
      gather Set.DayOfTheFeast
      gatherAndSetAside Set.AgentsOfTheColour
      gather Set.TheVale
      gatherAndSetAside Set.Residents

      setAgendaDeck [Agendas.allIsFullOfLove]
      setActDeck [Acts.dawnOfTheFinalDay]
      placeStory Stories.dayThree

      startAt =<< place Locations.boardingHouseDay
      theCrossroads <- place Locations.theCrossroadsMorning
      createAssetAt_ Assets.motherRachelKindlyMatron (AtLocation theCrossroads)
      createAssetAt_ Assets.judithParkTheMuscle (AtLocation theCrossroads)
      placeAll
        [ Locations.hemlockChapelDay
        , Locations.theOldMillMorning
        , Locations.theAtwoodHouseDay
        , Locations.tadsGeneralStoreDay
        , Locations.valeSchoolhouseDay
        , Locations.theCommonsDay
        ]

      doStep 1 Setup

      n <- getPlayerCount
      placeDoomOnAgenda n
    DoStep 1 Setup -> do
      getCrossedOutResidents >>= traverse_ (obtainCard <=< fetchCard)
      pure s
    -- TODO: survey gameplay (codex entries), resolutions, and EndOfScenario
    -- routing to the Day Three survey scenarios.
    _ -> PreludeDawnOfTheFinalDay <$> liftRunMessage msg attrs
