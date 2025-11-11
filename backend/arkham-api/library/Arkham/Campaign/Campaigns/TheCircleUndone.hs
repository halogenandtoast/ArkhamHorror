module Arkham.Campaign.Campaigns.TheCircleUndone (theCircleUndone, TheCircleUndone (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.CampaignSteps
import Arkham.Campaigns.TheCircleUndone.Import
import Arkham.Campaigns.TheCircleUndone.Memento.Helpers
import Arkham.Card
import Arkham.Card.PlayerCard (lookupPlayerCard)
import Arkham.ChaosToken
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Xp (XpBonus (WithBonus), toBonus)
import Arkham.I18n
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Matcher
import Arkham.Message (chooseDecks)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Projection
import Arkham.Question (Question (..))
import Arkham.Source
import Arkham.Trait (Trait (Curse, Omen, Pact, SilverTwilight))

newtype TheCircleUndone = TheCircleUndone CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign TheCircleUndone where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue DisappearanceAtTheTwilightEstate
    DisappearanceAtTheTwilightEstate -> continueNoUpgrade TheWitchingHour
    TheWitchingHour -> continue AtDeathsDoorstep
    AtDeathsDoorstep -> continue TheSecretName
    InterludeStep 2 _ -> continue TheSecretName
    TheSecretName -> continue TheWagesOfSin
    TheWagesOfSin -> continue ForTheGreaterGood
    ForTheGreaterGood -> continue UnionAndDisillusion
    InterludeStep 3 _ -> continue UnionAndDisillusion
    UnionAndDisillusion -> continue InTheClutchesOfChaos
    InTheClutchesOfChaos -> continue $ InterludeStep 4 Nothing
    InterludeStep 4 _ -> continue BeforeTheBlackThrone
    BeforeTheBlackThrone -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

theCircleUndone :: Difficulty -> TheCircleUndone
theCircleUndone =
  campaignWith TheCircleUndone (CampaignId "05") "The Circle Undone"
    $ logL
    .~ mkCampaignLog
      { campaignLogRecordedSets =
          singletonMap (toCampaignLogKey MissingPersons)
            $ map (recorded . cdCardCode) allPrologueInvestigators
      }

disappearanceAtTheTwilightEstateSteps :: [CampaignStep]
disappearanceAtTheTwilightEstateSteps =
  [ DisappearanceAtTheTwilightEstate
  , ReturnToDisappearanceAtTheTwilightEstate
  ]

instance RunMessage TheCircleUndone where
  runMessage msg c@(TheCircleUndone attrs) = runQueueT $ campaignI18n $ case msg of
    StartCampaign | attrs.step.unwrap `elem` (PrologueStep : disappearanceAtTheTwilightEstateSteps) -> do
      campaignStep_
        $ if attrs.step.unwrap `elem` disappearanceAtTheTwilightEstateSteps
          then PrologueStep
          else attrs.step.unwrap
      pure c
    CampaignStep TheWitchingHour -> do
      players <- allPlayers
      push $ chooseDecks players
      lift $ defaultCampaignRunner msg c
    CampaignStep ReturnToTheWitchingHour -> do
      players <- allPlayers
      push $ chooseDecks players
      lift $ defaultCampaignRunner msg c
    CampaignStep PrologueStep -> scope "prologue" do
      flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure c
    CampaignStep ((.unwrap) -> PrologueStep) -> do
      lead <- getActivePlayer
      push $ Ask lead ContinueCampaign
      pure c
    CampaignStep (ContinueCampaignStep ((.nextStep) -> DisappearanceAtTheTwilightEstate)) -> do
      lead <- getActivePlayer
      push $ Ask lead ContinueCampaign
      pure c
    CampaignStep (ContinueCampaignStep ((.nextStep) -> ReturnToDisappearanceAtTheTwilightEstate)) -> do
      lead <- getActivePlayer
      push $ Ask lead ContinueCampaign
      pure c
    CampaignStep (InterludeStep 2 mInterludeKey) -> scope "interlude2" do
      anySilverTwilight <- selectAny $ InvestigatorWithTrait SilverTwilight
      let
        showThePriceOfProgress4 = mInterludeKey == Just ThePriceOfProgress4
        showThePriceOfProgress5 = mInterludeKey == Just ThePriceOfProgress5
        showThePriceOfProgress6 = mInterludeKey == Just ThePriceOfProgress6
        lodgeChoices = do
          labeled' "refuse" $ interludeStepPart 2 mInterludeKey 7
          labeled' "agree" $ interludeStepPart 2 mInterludeKey 8
          labeled' "lie" $ interludeStepPart 2 mInterludeKey 9

      flavor $ setTitle "title" >> p "instructions"
      flavor
        $ setTitle "title"
        >> p (if anySilverTwilight then "thePriceOfProgress1" else "thePriceOfProgress2")
      flavor $ setTitle "title" >> p "thePriceOfProgress3"

      when showThePriceOfProgress4 do
        flavor $ setTitle "title" >> p "thePriceOfProgress4"
        record JosefDisappearedIntoTheMist
        record TheInvestigatorsAreEnemiesOfTheLodge
        whenM (getHasRecord YouAreBeingHunted) do
          flavor $ p "youAreBeingHunted"
        nextCampaignStep

      when showThePriceOfProgress5 do
        record TheInvestigatorsRescuedJosef
        interludeXpAll (WithBonus "Gained insight into the inner workings of the Silver Twilight Lodge" 2)
        storyWithChooseOneM' (setTitle "title" >> p "thePriceOfProgress5") lodgeChoices

      when showThePriceOfProgress6 do
        record JosefIsAliveAndWell
        storyWithChooseOneM' (setTitle "title" >> p "thePriceOfProgress6") lodgeChoices
      pure c
    CampaignStep (InterludeStepPart 2 _ 7) -> scope "interlude2" do
      flavor $ setTitle "title" >> p "thePriceOfProgress7"
      record TheInvestigatorsAreEnemiesOfTheLodge
      whenM (getHasRecord YouAreBeingHunted) do
        flavor $ p "youAreBeingHunted"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 2 _ 8) -> scope "interlude2" do
      flavor $ setTitle "title" >> p "thePriceOfProgress8"
      record TheInvestigatorsAreMembersOfTheLodge
      addChaosToken Cultist
      whenM (getHasRecord YouAreBeingHunted) do
        flavor $ p "youAreBeingHunted"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 2 _ 9) -> scope "interlude2" do
      flavor $ setTitle "title" >> p "thePriceOfProgress9"
      record TheInvestigatorsAreMembersOfTheLodge
      addChaosToken Cultist
      record TheInvestigatorsAreDeceivingTheLodge
      whenM (getHasRecord YouAreBeingHunted) do
        flavor $ p "youAreBeingHunted"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 3 mInterludeKey) -> scope "interlude3" do
      storyWithChooseOneM' (setTitle "title" >> p "theInnerCircle1") do
        labeled' "truth" $ interludeStepPart 3 mInterludeKey 2
        labeled' "lie" $ interludeStepPart 3 mInterludeKey 3
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 2) -> scope "interlude3" do
      rescuedJosef <- getHasRecord TheInvestigatorsRescuedJosef
      toldLodgeAboutCoven <- getHasRecord TheInvestigatorsToldTheLodgeAboutTheCoven
      someMementos <- getRecordSet MementosDiscovered
      let mementos = mapMaybe (unrecorded @Memento) someMementos
      flavor $ setTitle "title" >> p "theInnerCircle2"
      crossOutRecordSetEntries MementosDiscovered (toList mementos)
      interludeStepPart 3 mInterludeKey $ if rescuedJosef && toldLodgeAboutCoven then 4 else 5
      pure c
    CampaignStep (InterludeStepPart 3 _ 3) -> scope "interlude3" do
      flavor $ setTitle "title" >> p "theInnerCircle3"
      record TheInvestigatorsKeptsTheirMementosHidden
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 4) -> scope "interlude3" do
      survived <- getHasRecord TheInvestigatorsSurvivedTheWatchersEmbrace
      storyWithChooseUpToNM' 3 "done" (setTitle "title" >> p "theInnerCircle4") do
        labeled' "whatIsTheCreature" $ flavor $ p.green "whatIsTheCreature"
        labeled' "whatDoYouWantWithTheCreature" $ flavor $ p.green "whatDoYouWantWithTheCreature"
        labeled' "whatDoTheWitchesWantWithTheCreature"
          $ flavor
          $ p.green "whatDoTheWitchesWantWithTheCreature"
        labeled' "didYouKnowAboutTheCreature"
          $ flavor
          $ p.green "didYouKnowAboutTheCreatureBeforeTheCharityGala"
        labeled' "whereAreTheFourMissingPeople"
          $ flavor
          $ p.green "whereAreTheFourMissingPeopleFromTheCharityGala"
        when survived
          $ labeled' "whyAreYouLookingAtMeLikeThat"
          $ flavor
          $ p.green "whyAreYouLookingAtMeLikeThat"
      interludeStepPart 3 mInterludeKey 6
      pure c
    CampaignStep (InterludeStepPart 3 _ 5) -> scope "interlude3" do
      flavor $ setTitle "title" >> p "theInnerCircle5"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 3 _ 6) -> scope "interlude3" do
      flavor $ setTitle "title" >> p "theInnerCircle6"
      record TheInvestigatorsWereInductedIntoTheInnerCircle
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 4 _) -> scope "interlude4" do
      acceptedYourFate <- getHasRecord YouHaveAcceptedYourFate
      askedAnetteForAssistance <- getHasRecord TheInvestigatorsAskedAnetteForAssistance
      askedSanfordForAssistance <- getHasRecord TheInvestigatorsAskedSanfordForAssistance
      mementosDiscovered <- getMementosDiscoveredCount
      doomDrawsEverCloser <- getHasRecord DoomDrawsEverCloser
      hasBlackBook <- isJust <$> getOwner Assets.theBlackBook

      arrestedAnette <- getHasRecord TheInvestigatorsArrestedAnette
      arrestedSanford <- getHasRecord TheInvestigatorsArrestedSanford
      anetteTaughtYouTheSpellsOfOld <- getHasRecord AnetteTaughtYouTheSpellsOfOld
      assumedControlOfTheLodge <- getHasRecord TheInvestigatorsAssumedControlOfTheSilverTwilightLodge

      let isReturnTo = attrs.id == "54"
      let
        total =
          max 0
            $ getSum
            $ mconcat
              [ mwhen acceptedYourFate (Sum 1)
              , mwhen askedAnetteForAssistance (Sum 2)
              , mwhen askedSanfordForAssistance (Sum 2)
              , mwhen (mementosDiscovered >= 3) (Sum 1)
              , mwhen (mementosDiscovered >= 6) (Sum 1)
              , mwhen (mementosDiscovered >= 9) (Sum 1)
              , mwhen hasBlackBook (Sum 1)
              , mwhen doomDrawsEverCloser (Sum 2)
              , mwhen (arrestedAnette && isReturnTo) (Sum (-1))
              , mwhen (arrestedSanford && isReturnTo) (Sum (-1))
              ]

      flavor do
        setTitle "title"
        p "twistOfFate1"
        ul do
          li.nested.validate acceptedYourFate "acceptedYourFate" do
            li "youKnowWhatYouHaveToDo"
          li.nested.validate askedAnetteForAssistance "askedAnetteForAssistance" do
            li "anettesAssistance"
            li "adjustChaosBag"
          li.nested.validate askedSanfordForAssistance "askedSanfordForAssistance" do
            li "sanfordsAssistance"
            li "adjustChaosBag"
          li.nested.validate (mementosDiscovered <= 2) "twoOrFewerMementos" do
            li "woefullyUnprepared"
          li.nested.validate (mementosDiscovered >= 3 && mementosDiscovered <= 5) "threeToFiveMementos" do
            li "thereIsStillMuchYouDoNotKnow"
          li.nested.validate (mementosDiscovered >= 6) "sixOrMoreMementos" do
            li "yourJourneyIsLaidBareBeforeYou"
          li.nested.validate hasBlackBook "haveTheBlackBook" do
            li "theBlackBook"
          li.nested.validate doomDrawsEverCloser "doomDrawsEverCloser" do
            li "azathothsMawBeginsToOpen"
          when isReturnTo do
            li.nested.validate arrestedAnette "arrestedAnette" do
              li "anetteIsInCustody"
            li.nested.validate anetteTaughtYouTheSpellsOfOld "anetteTaughtYouTheSpellsOfOld" do
              li "learnedTheSpellsOfOld"
            li.nested.validate arrestedSanford "arrestedSanford" do
              li "sanfordIsInCustody"
            li.nested.validate assumedControlOfTheLodge "assumedControlOfTheLodge" do
              li "lodgeUsurped"

      recordCount ThePathWindsBeforeYou total
      when (askedAnetteForAssistance || askedSanfordForAssistance) do
        addChaosToken $ fromDifficulty MinusThree MinusFour MinusFive MinusSix attrs.difficulty
      when isReturnTo do
        when arrestedAnette $ removeChaosToken Skull
        when arrestedSanford $ removeChaosToken Skull
        when assumedControlOfTheLodge do
          investigators <- allInvestigators
          for_ investigators \iid -> do
            scenarioSetupModifier "54056" CampaignSource iid (StartingResources 2)
            scenarioSetupModifier "54056" CampaignSource iid (StartingHand 2)
        when anetteTaughtYouTheSpellsOfOld do
          investigators <- allInvestigators
          playerCount <- getPlayerCount
          let
            multiplayerFilter =
              if playerCount < 2
                then notElem MultiplayerOnly . cdDeckRestrictions . toCardDef
                else const True
          let collection = map (`lookupPlayerCard` nullCardId) (toList allPlayerCards)
          for_ investigators \iid -> do
            chooseOneM iid do
              campaignI18n $ scope "returnTo" $ questionLabeled' "anetteTaughtYouTheSpellsOfOld"
              withI18n $ labeled' "yes" do
                campaignI18n $ scope "returnTo" $ interludeXp iid $ toBonus "bonus" 4
                investigatorClass <- field Investigator.InvestigatorClass iid

                let
                  notForClass = \case
                    OnlyClass c' -> c' /= investigatorClass
                    _ -> True
                  classOnlyFilter = not . any notForClass . cdDeckRestrictions . toCardDef
                  cardFilter =
                    and
                      . sequence
                        [ multiplayerFilter
                        , classOnlyFilter
                        , ( `cardMatch`
                              (BasicWeaknessCard <> mapOneOf CardWithTrait [Pact, Curse, Omen, SilverTwilight])
                          )
                        ]
                  cards = filter cardFilter collection
                chooseOneM iid do
                  cardsLabeled cards $ addCampaignCardToDeck iid DoNotShuffleIn

              withI18n $ labeled' "no" nothing

      flavor $ setTitle "title" >> p "twistOfFate2"
      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> scope "epilogue" do
      whenHasRecord TheInvestigatorsArrestedAnette do
        flavor $ setTitle "title" >> p.green "arrestedAnette"
      whenHasRecord TheInvestigatorsAssumedControlOfTheSilverTwilightLodge do
        flavor $ setTitle "title" >> p.green "assumedControlOfTheLodge"
      whenHasRecord TheInvestigatorsSurvivedTheWatchersEmbrace do
        flavor $ setTitle "title" >> p.green "survivedTheWatchersEmbrace"
        crossOut TheInvestigatorsSurvivedTheWatchersEmbrace
      whenHasRecord TheInvestigatorsSignedTheBlackBookOfAzathoth do
        flavor $ setTitle "title" >> p.green "signedTheBlackBook"
      whenHasRecord YouAreBeingHunted do
        flavor $ setTitle "title" >> p "youAreBeingHunted"
      gameOver

      pure c
    HandleOption option -> do
      investigators <- allInvestigators
      case option of
        TakeBlackBook -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.theBlackBook
        TakePuzzleBox -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.puzzleBox
        ProceedToInterlude3 -> pure ()
        _ -> error $ "Unhandled option: " <> show option
      pure c
    _ -> lift $ defaultCampaignRunner msg c
