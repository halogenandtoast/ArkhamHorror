module Arkham.Homebrew.CircusExMortis.Campaign (circusExMortis) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as HBAssets
import Arkham.Campaign.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CampaignSteps
import Arkham.Homebrew.CircusExMortis.ChaosBag
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Helpers.Campaign (getCompletedSteps, getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith, setActiveDuringSetup)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Homebrew.CircusExMortis.CardDefs.Skills qualified as Skills
import Arkham.Source
import Arkham.Target (Target (GameTarget))
import Arkham.Trait (Trait (Believer, Chosen, Clairvoyant, Miskatonic, Scholar))

newtype CircusExMortis = CircusExMortis CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusExMortis :: Difficulty -> CircusExMortis
circusExMortis = campaign CircusExMortis (CampaignId ":circus-ex-mortis") "Circus Ex Mortis"

instance IsCampaign CircusExMortis where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue OneNightOnly
    OneNightOnly -> continue ThePrimrosePath
    ThePrimrosePath -> continue TheFutureAndThePast
    TheFutureAndThePast -> continue HarmsWay
    HarmsWay -> continue AllPointsWest
    AllPointsWest -> continue WrittenInStone
    WrittenInStone -> continue PiperAtTheGatesOfDawn
    PiperAtTheGatesOfDawn -> continue Bacchanalia
    Bacchanalia -> continue GoodOmens
    GoodOmens -> continue RedSunrise
    RedSunrise -> continue ThousandToOne
    ThousandToOne -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

instance HasModifiersFor CircusExMortis where
  getModifiersFor (CircusExMortis attrs) = do
    -- Interlude "The Future and the Past", Lingua Franca (guide p10):
    -- Miskatonic, Scholar, or Believer investigators begin the next scenario
    -- (Harm's Way) with 1 additional card in their opening hand.
    when (attrs.normalizedStep == HarmsWay) do
      modifySelectWith
        CampaignSource
        (mapOneOf InvestigatorWithTrait [Miskatonic, Scholar, Believer])
        setActiveDuringSetup
        [StartingHand 1]

instance RunMessage CircusExMortis where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      scope "additionalRules" $ flavor $ setTitle "title" >> p "moonTokens"
      scope "prologue" do
        flavor $ setTitle "title" >> p "body"
        flavor do
          setTitle "title"
          p "minnie"
          ul $ li "addMoonTokens"
      replicateM_ 3 $ addChaosToken MoonToken
      nextCampaignStep
      pure c
    -- Interlude: The Future and the Past (guide pp9-10)
    CampaignStep (InterludeStep 1 _) -> scope "theFutureAndThePast" do
      flavor $ setTitle "title" >> p "intro"
      -- Far-Seeing: 1 bonus xp for each Clairvoyant investigator; the guide
      -- restricts it to purchasing Augury cards (not enforced by the app).
      clairvoyants <- select $ InvestigatorWithTrait Clairvoyant
      unless (null clairvoyants) do
        scope "farSeeing" $ flavor $ setTitle "title" >> p "body"
        for_ clairvoyants \iid -> interludeXp iid $ toBonus "farSeeing" 1
      flavor do
        setTitle "title"
        p "thea"
        ul $ li "addAmaltheaWeaver"
      addCampaignCardToDeckChoice_ HBAssets.amaltheaWeaverCircusFortuneTeller
      flavor $ setTitle "title" >> p "theTome"
      linguists <- select $ mapOneOf InvestigatorWithTrait [Miskatonic, Scholar, Believer]
      unless (null linguists) do
        -- The +1 opening hand for the next scenario is applied via
        -- HasModifiersFor while the campaign step is Harm's Way.
        scope "linguaFranca" $ flavor $ setTitle "title" >> p "body"
      flavor do
        setTitle "title"
        p "apuleius"
        ul $ li "addDeCultusBestiae"
      addCampaignCardToDeckChoice_ HBAssets.deCultusBestiaeForgottenWorkOfApuleius
      flavor $ setTitle "title" >> p "eclipse"
      normans <- select $ InvestigatorWithTitle "Norman Withers"
      unless (null normans) do
        -- Like Clockwork grants Norman "up to 2 additional Seeker cards
        -- (level 1-2)" in deckbuilding; deck construction is external to the
        -- engine, so this is informational.
        scope "likeClockwork" $ flavor $ setTitle "title" >> p "body"
      flavor do
        setTitle "title"
        p "escape"
        ul $ li "addMoonToken"
      addChaosToken MoonToken
      nextCampaignStep
      pure c
    -- Interlude: Written in Stone (guide pp19-20)
    CampaignStep (InterludeStep 2 _) -> scope "writtenInStone" do
      flavor $ setTitle "title" >> p "intro"
      flavor do
        setTitle "title"
        p "prophecy"
        ul $ li "addInvocationOfDiana"
      eachInvestigator \iid -> do
        chooseOneM iid do
          questionLabeledCard iid
          questionLabeled' "addInvocationOfDianaQuestion"
          labeled' "addInvocationOfDiana"
            $ addCampaignCardToDeck iid DoNotShuffleIn Skills.invocationOfDiana
          labeled' "doNotAddInvocationOfDiana" nothing
      storyWithChooseOneM' (setTitle "title" >> p "role") do
        labeled' "determination" do
          flavor $ setTitle "title" >> p "determination"
          swapCampaignCard
            HBAssets.amaltheaWeaverCircusFortuneTeller
            HBAssets.amaltheaWeaverAspirantOfCourage
        labeled' "guidance" do
          flavor $ setTitle "title" >> p "guidance"
          swapCampaignCard
            HBAssets.amaltheaWeaverCircusFortuneTeller
            HBAssets.amaltheaWeaverAspirantOfWisdom
      scope "furtherReading" $ flavor $ setTitle "title" >> p "body"
      chosen <- select $ InvestigatorWithTrait Chosen
      unless (null chosen) do
        scope "noChoice" $ flavor $ setTitle "title" >> p "body"
        for_ chosen \iid -> do
          hasPhysical <- fieldP InvestigatorPhysicalTrauma (> 0) iid
          hasMental <- fieldP InvestigatorMentalTrauma (> 0) iid
          when (hasPhysical || hasMental) do
            chooseOneM iid do
              questionLabeledCard iid
              questionLabeled' "healTraumaQuestion"
              when hasPhysical $ labeled' "healPhysicalTrauma" $ push $ HealTrauma iid 1 0
              when hasMental $ labeled' "healMentalTrauma" $ push $ HealTrauma iid 0 1
              labeled' "doNotHealTrauma" nothing
      storyWithChooseOneM' (setTitle "title" >> p "motive") do
        labeled' "fanaticism" do
          flavor $ setTitle "title" >> p "fanaticism"
          swapCampaignCard
            HBAssets.deCultusBestiaeForgottenWorkOfApuleius
            HBAssets.deCultusBestiaeInterpretationOfConviction
        labeled' "nemesis" do
          flavor $ setTitle "title" >> p "nemesis"
          swapCampaignCard
            HBAssets.deCultusBestiaeForgottenWorkOfApuleius
            HBAssets.deCultusBestiaeInterpretationOfObsession
      flavor do
        setTitle "title"
        p "bookmark"
        ul $ li "addSkullToken"
      addChaosToken #skull
      nextCampaignStep
      pure c
    -- Interlude: Good Omens (guide pp27-28)
    CampaignStep (InterludeStep 3 _) -> scope "goodOmens" do
      flavor $ setTitle "title" >> p "intro"
      mAmalthea <- getAmaltheaWeaverOwner
      case snd <$> mAmalthea of
        Just v
          | v == HBAssets.amaltheaWeaverAspirantOfCourage ->
              storyWithChooseOneM' (setTitle "title" >> p "moreToDo") do
                labeled' "priorWarning" do
                  flavor $ setTitle "title" >> p "priorWarning"
                  swapCampaignCard v HBAssets.amaltheaWeaverOracleOfPurity
                labeled' "sawItComing" do
                  flavor $ setTitle "title" >> p "sawItComing"
                  swapCampaignCard v HBAssets.amaltheaWeaverOracleOfResolve
        Just v
          | v == HBAssets.amaltheaWeaverAspirantOfWisdom ->
              storyWithChooseOneM' (setTitle "title" >> p "moreToSee") do
                labeled' "writtenInInk" do
                  flavor $ setTitle "title" >> p "writtenInInk"
                  swapCampaignCard v HBAssets.amaltheaWeaverOracleOfEnlightenment
                labeled' "writtenInSmoke" do
                  flavor $ setTitle "title" >> p "writtenInSmoke"
                  swapCampaignCard v HBAssets.amaltheaWeaverOracleOfMystery
        _ -> pure ()
      scope "theLastWord" $ flavor $ setTitle "title" >> p "body"
      mDeCultus <- getDeCultusBestiaeOwner
      case snd <$> mDeCultus of
        Just v
          | v == HBAssets.deCultusBestiaeInterpretationOfConviction ->
              storyWithChooseOneM' (setTitle "title" >> p "theInfinite") do
                labeled' "powersAbove" do
                  flavor $ setTitle "title" >> p "powersAbove"
                  swapCampaignCard v HBAssets.deCultusBestiaeProphecyOfTheBeyond
                labeled' "powersBelow" do
                  flavor $ setTitle "title" >> p "powersBelow"
                  swapCampaignCard v HBAssets.deCultusBestiaeProphecyOfTheEternal
        Just v
          | v == HBAssets.deCultusBestiaeInterpretationOfObsession ->
              storyWithChooseOneM' (setTitle "title" >> p "theEndless") do
                labeled' "againstTheFlood" do
                  flavor $ setTitle "title" >> p "againstTheFlood"
                  swapCampaignCard v HBAssets.deCultusBestiaeProphecyOfTheHorde
                labeled' "againstTheStorm" do
                  flavor $ setTitle "title" >> p "againstTheStorm"
                  swapCampaignCard v HBAssets.deCultusBestiaeProphecyOfTheBehemoth
        _ -> pure ()
      flavor do
        setTitle "title"
        p "breakOfDawn"
        ul $ li "addSkullToken"
      addChaosToken #skull
      nextCampaignStep
      pure c
    -- Epilogue (guide pp35-36); only reached when the investigators won.
    CampaignStep EpilogueStep -> scope "epilogue" do
      flavor $ setTitle "title" >> p "intro"
      getOwner Assets.monstrousTransformation >>= traverse_ \_ ->
        scope "underWraps" $ flavor $ setTitle "title" >> p "body"
      getOwner Assets.ladyEsprit >>= traverse_ \_ ->
        scope "divergingPaths" $ flavor $ setTitle "title" >> p "body"
      flavor $ setTitle "title" >> p "thea"
      prophecyFulfilled <- getHasRecord TheProphecyWasFulfilled
      if prophecyFulfilled
        then do
          scope "kernelOfTruth" $ flavor $ setTitle "title" >> p "body"
          interludeXpAll $ toBonus "kernelOfTruth" 2
        else scope "grainOfSalt" $ flavor $ setTitle "title" >> p "body"
      scope "lookingAhead" $ flavor $ setTitle "title" >> p "body"
      clashed <- getHasRecord TheInvestigatorsClashedWithBlake
      unmasked <- getHasRecord TheInvestigatorsUnmaskedBlake
      rallied <- getHasRecord TheCultRallies
      if (clashed || unmasked) && rallied
        then do
          scope "encore" $ flavor $ setTitle "title" >> p "body"
          record TheNewMoonCircusMaySomedayReturn
          getAmaltheaWeaverOwner >>= traverse_ \(iid, _) ->
            addCampaignCardToDeck iid DoNotShuffleIn Assets.theTowerXVI
          getDeCultusBestiaeOwner >>= traverse_ \(iid, _) ->
            addCampaignCardToDeck iid DoNotShuffleIn Assets.theDevilXv
        else do
          scope "restAssured" $ flavor $ setTitle "title" >> p "body"
          record TheNewMoonCircusWasNeverSeenAgain
      push GameOver
      pure c
    -- Curse of the Rougarou between Harm's Way and All Points West costs each
    -- investigator 1 fewer experience (guide p13). Its base side-story cost is
    -- 1, so it is free: mirror the default runner minus the XP spend.
    CampaignStep (StandaloneScenarioStep sid _) | sid == curseOfTheRougarouId -> do
      afterHarmsWay <- isAfterHarmsWay
      if afterHarmsWay
        then startRougarouWithoutXp sid Nothing
        else lift $ defaultCampaignRunner msg c
    CampaignStep (StandaloneScenarioStepWithOptions sid _ opts) | sid == curseOfTheRougarouId -> do
      afterHarmsWay <- isAfterHarmsWay
      if afterHarmsWay
        then startRougarouWithoutXp sid (Just opts)
        else lift $ defaultCampaignRunner msg c
    -- Moon tokens, end of round (guide p1): for each moon token sealed on
    -- your investigator card, you must choose to keep it sealed or take 1
    -- damage or 1 horror and release it.
    EndRoundWindow -> scope "moonToken" do
      eachInvestigator \iid -> do
        tokens <- getSealedMoonTokens iid
        for_ tokens \token -> do
          chooseOneM iid do
            questionLabeledCard iid
            questionLabeled' "endOfRoundQuestion"
            labeled' "keepSealed" nothing
            labeled' "takeDamageAndRelease" do
              assignDamage iid CampaignSource 1
              releaseMoonToken token
            labeled' "takeHorrorAndRelease" do
              assignHorror iid CampaignSource 1
              releaseMoonToken token
      pure c
    _ -> lift $ defaultCampaignRunner msg c
   where
    isAfterHarmsWay = do
      completed <- getCompletedSteps
      -- completed steps are most-recent-first; the head scenario must be
      -- Harm's Way for the discount to apply
      pure $ listToMaybe (filter (isJust . (.scenario)) completed) == Just HarmsWay
    startRougarouWithoutXp sid opts = do
      pushAll
        [ ResetInvestigators
        , ResetGame
        , ForTarget GameTarget ResetGame
        , ForInvestigators [] ResetGame
        , StartScenario sid opts
        ]
      pure c
