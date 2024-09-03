{-# LANGUAGE MultiWayIf #-}

module Arkham.Campaign.Campaigns.TheDreamEaters (TheDreamEaters (..), theDreamEaters) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Runner hiding (story, storyWithChooseOne)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheDreamEaters.Import
import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign hiding (addCampaignCardToDeckChoice)
import Arkham.Helpers.Log hiding (getHasRecord)
import Arkham.Helpers.Log qualified as Lift
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Investigator (Investigator, lookupInvestigator)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted hiding (record)
import Arkham.Prelude
import Arkham.Projection
import Data.Aeson (Result (..))

newtype TheDreamEaters = TheDreamEaters CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theDreamEaters :: Difficulty -> TheDreamEaters
theDreamEaters difficulty =
  campaignWith TheDreamEaters (CampaignId "06") "The Dream-Eaters" difficulty []
    $ metaL
    .~ toJSON (Metadata FullMode Nothing Nothing mempty mempty)

recordInBoth :: HasQueue Message m => CampaignLogKey -> m ()
recordInBoth = pushBoth . Record

pushBoth :: HasQueue Message m => Message -> m ()
pushBoth msg = pushAll [InTheWebOfDreams msg, InTheDreamQuest msg]

record :: HasQueue Message m => CampaignPart -> CampaignLogKey -> m ()
record TheDreamQuest key = push $ InTheDreamQuest (Record key)
record TheWebOfDreams key = push $ InTheWebOfDreams (Record key)

setCampaignPart
  :: ReverseQueue m
  => CampaignPart
  -> TheDreamEaters
  -> CampaignStep
  -> m TheDreamEaters
setCampaignPart part c@(TheDreamEaters attrs) step =
  if (toResult @Metadata attrs.meta).mode == PartialMode part
    then do
      push $ CampaignStep step
      pure c
    else do
      investigators <- allInvestigators
      currentPlayers <- for investigators \i -> do
        player <- getPlayer i
        iattrs <- getAttrs @Investigator i
        pure (player, iattrs)

      let meta = toResult @Metadata attrs.meta
          newAttrs = fromJustNote "not full campaign" (otherCampaignAttrs meta)
          newMeta =
            meta
              { currentCampaignMode = Just part
              , otherCampaignAttrs = Just $ attrs {campaignMeta = Null}
              , currentCampaignPlayers = meta.otherCampaignPlayers
              , otherCampaignPlayers = mapFromList currentPlayers
              }

      for_ (mapToList $ otherCampaignPlayers meta) \(pid, iattrs) -> do
        let i = overAttrs (const iattrs) $ lookupInvestigator (toId iattrs) pid
        push $ SetInvestigator pid i

      push $ CampaignStep step

      pure
        $ TheDreamEaters
          ( newAttrs
              { campaignCompletedSteps = campaignCompletedSteps attrs
              , campaignStep = campaignStep attrs
              , campaignMeta = toJSON newMeta
              }
          )

getHasRecord :: HasGame m => CampaignPart -> CampaignLogKey -> m Bool
getHasRecord part key = do
  isCurrent <- getIsPartialCampaign part
  if isCurrent
    then Lift.getHasRecord key
    else do
      meta <- getCampaignMeta @Metadata
      if meta.currentCampaignMode == Just part
        then Lift.getHasRecord key
        else case meta.otherCampaignAttrs of
          Just otherAttrs -> pure $ hasRecord key otherAttrs.log
          Nothing -> error "no other campaign attrs"

instance IsCampaign TheDreamEaters where
  nextStep a@(TheDreamEaters attrs) =
    let meta = toResult (campaignMeta attrs)
     in case campaignStep (toAttrs a) of
          PrologueStep -> error $ "Unhandled campaign step: " <> show a
          BeyondTheGatesOfSleep ->
            Just
              ( UpgradeDeckStep
                  $ case campaignMode meta of
                    FullMode ->
                      if WakingNightmare `elem` campaignCompletedSteps (toAttrs a)
                        then InterludeStep 1 Nothing
                        else WakingNightmare
                    PartialMode _ -> InterludeStep 1 Nothing
              )
          WakingNightmare ->
            Just
              ( UpgradeDeckStep
                  $ case campaignMode meta of
                    FullMode ->
                      if BeyondTheGatesOfSleep `elem` campaignCompletedSteps (toAttrs a)
                        then InterludeStep 1 Nothing
                        else BeyondTheGatesOfSleep
                    PartialMode _ -> InterludeStep 1 Nothing
              )
          InterludeStep 1 _ -> error $ "Unhandled campaign step: " <> show a
          TheSearchForKadath ->
            Just
              ( UpgradeDeckStep
                  $ case campaignMode meta of
                    FullMode ->
                      if AThousandShapesOfHorror `elem` campaignCompletedSteps (toAttrs a)
                        then InterludeStep 2 Nothing
                        else AThousandShapesOfHorror
                    PartialMode _ -> DarkSideOfTheMoon
              )
          AThousandShapesOfHorror ->
            Just
              ( UpgradeDeckStep
                  $ case campaignMode meta of
                    FullMode ->
                      if TheSearchForKadath `elem` campaignCompletedSteps (toAttrs a)
                        then InterludeStep 2 Nothing
                        else TheSearchForKadath
                    PartialMode _ -> PointOfNoReturn
              )
          InterludeStep 2 _ -> error $ "Unhandled campaign step: " <> show a
          DarkSideOfTheMoon ->
            Just
              ( UpgradeDeckStep
                  $ case campaignMode meta of
                    FullMode ->
                      if PointOfNoReturn `elem` campaignCompletedSteps (toAttrs a)
                        then InterludeStep 3 Nothing
                        else PointOfNoReturn
                    PartialMode _ -> WhereTheGodsDwell
              )
          PointOfNoReturn ->
            Just
              ( UpgradeDeckStep
                  $ case campaignMode meta of
                    FullMode ->
                      if DarkSideOfTheMoon `elem` campaignCompletedSteps (toAttrs a)
                        then InterludeStep 3 Nothing
                        else DarkSideOfTheMoon
                    PartialMode _ -> WeaverOfTheCosmos
              )
          InterludeStep 3 _ -> error $ "Unhandled campaign step: " <> show a
          WhereTheGodsDwell ->
            Just
              ( case campaignMode meta of
                  FullMode ->
                    if WeaverOfTheCosmos `elem` campaignCompletedSteps (toAttrs a)
                      then EpilogueStep
                      else WeaverOfTheCosmos
                  PartialMode _ -> EpilogueStep
              )
          WeaverOfTheCosmos ->
            Just
              ( case campaignMode meta of
                  FullMode ->
                    if WhereTheGodsDwell `elem` campaignCompletedSteps (toAttrs a)
                      then EpilogueStep
                      else WhereTheGodsDwell
                  PartialMode _ -> EpilogueStep
              )
          EpilogueStep -> Nothing
          UpgradeDeckStep nextStep' -> Just nextStep'
          _ -> Nothing

theDreamQuestSteps :: [CampaignStep]
theDreamQuestSteps = [BeyondTheGatesOfSleep, TheSearchForKadath, DarkSideOfTheMoon, WhereTheGodsDwell]

theWebOfDreamsSteps :: [CampaignStep]
theWebOfDreamsSteps = [WakingNightmare, AThousandShapesOfHorror, PointOfNoReturn, WeaverOfTheCosmos]

instance RunMessage TheDreamEaters where
  runMessage msg c@(TheDreamEaters attrs) = runQueueT do
    let
      meta = case fromJSON (campaignMeta attrs) of
        Success a -> a
        _ -> error "Could not read Metadata"

    case msg of
      StartCampaign -> do
        -- [ALERT] StartCampaign, overriden to not choose decks yet
        lead <- getActivePlayer
        pushAll
          $ [Ask lead PickCampaignSettings | campaignStep attrs /= PrologueStep]
          <> [CampaignStep $ campaignStep attrs]
        pure c
      CampaignStep PrologueStep -> do
        lead <- getActivePlayer
        story prologue
        push
          $ Msg.questionLabel "Which mode would you like to play" lead
          $ ChooseOne
            [ Label "Full Campaign" [CampaignStep (PrologueStepPart 1)]
            , Label "The Dream-Quest" [CampaignStep (PrologueStepPart 2)]
            , Label "The Web of Dreams" [CampaignStep (PrologueStepPart 3)]
            ]
        pure c
      CampaignStep (PrologueStepPart 1) -> do
        lead <- getActivePlayer
        push
          $ Msg.questionLabel "Which scenario would you like to start with" lead
          $ ChooseOne
            [ Label "Beyond the Gates of Sleep" [CampaignStep (PrologueStepPart 11)]
            , Label "Waking Nightmare" [CampaignStep (PrologueStepPart 12)]
            ]
        pure c
      CampaignStep (PrologueStepPart 2) -> do
        players <- allPlayers
        pushAll
          [ chooseDecks players
          , NextCampaignStep (Just BeyondTheGatesOfSleep)
          ]
        pure
          $ TheDreamEaters
          $ attrs
            { campaignChaosBag = initChaosBag TheDreamQuest (campaignDifficulty attrs)
            , campaignMeta = toJSON $ meta {campaignMode = PartialMode TheDreamQuest}
            }
      CampaignStep (PrologueStepPart 3) -> do
        players <- allPlayers
        pushAll
          [ chooseDecks players
          , NextCampaignStep (Just WakingNightmare)
          ]
        pure
          $ TheDreamEaters
          $ attrs
            { campaignChaosBag = initChaosBag TheWebOfDreams (campaignDifficulty attrs)
            , campaignMeta = toJSON $ meta {campaignMode = PartialMode TheWebOfDreams}
            }
      CampaignStep (PrologueStepPart 11) -> do
        players <- allPlayers
        pushAll
          $ ChoosingDecks
          : map (\pid -> Msg.questionLabel "Choose Deck For Part A" pid ChooseDeck) players
            <> [DoneChoosingDecks, NextCampaignStep (Just BeyondTheGatesOfSleep)]
        let difficulty = campaignDifficulty attrs
        pure
          $ TheDreamEaters
          $ attrs
            { campaignChaosBag = initChaosBag TheDreamQuest difficulty
            , campaignMeta =
                toJSON
                  $ meta
                    { currentCampaignMode = Just TheDreamQuest
                    , otherCampaignAttrs = Just (attrs {campaignChaosBag = initChaosBag TheWebOfDreams difficulty})
                    }
            }
      CampaignStep (PrologueStepPart 12) -> do
        players <- allPlayers
        pushAll
          $ ChoosingDecks
          : map (\pid -> Msg.questionLabel "Choose Deck For Part B" pid ChooseDeck) players
            <> [DoneChoosingDecks, NextCampaignStep (Just WakingNightmare)]
        let difficulty = campaignDifficulty attrs
        pure
          $ TheDreamEaters
          $ attrs
            { campaignChaosBag = initChaosBag TheWebOfDreams difficulty
            , campaignMeta =
                toJSON
                  $ meta
                    { currentCampaignMode = Just TheWebOfDreams
                    , otherCampaignAttrs = Just (attrs {campaignChaosBag = initChaosBag TheDreamQuest difficulty})
                    }
            }
      CampaignStep s@(ScenarioStep _) -> do
        c' <- case s of
          _ | s `elem` theDreamQuestSteps -> do
            if currentCampaignMode meta == Just TheWebOfDreams
              then do
                investigators <- allInvestigators
                currentPlayers <- for investigators \i -> do
                  player <- getPlayer i
                  iattrs <- getAttrs @Investigator i
                  pure (player, iattrs)

                if s == BeyondTheGatesOfSleep && WakingNightmare `elem` campaignCompletedSteps attrs
                  then do
                    players <- allPlayers
                    pushAll
                      $ ChoosingDecks
                      : map (\pid -> Msg.questionLabel "Choose Deck For Part A" pid ChooseDeck) players
                        <> [DoneChoosingDecks]
                  else do
                    for_ (mapToList $ otherCampaignPlayers meta) \(pid, iattrs) -> do
                      let i = overAttrs (const iattrs) $ lookupInvestigator (toId iattrs) pid
                      push $ SetInvestigator pid i
                let newAttrs = fromJustNote "not full campaign" (otherCampaignAttrs meta)
                pure
                  $ TheDreamEaters
                    ( newAttrs
                        { campaignCompletedSteps = campaignCompletedSteps attrs
                        , campaignStep = s
                        , campaignMeta =
                            toJSON
                              $ meta
                                { currentCampaignMode = Just TheDreamQuest
                                , otherCampaignAttrs = Just $ attrs {campaignMeta = Null}
                                , currentCampaignPlayers = otherCampaignPlayers meta
                                , otherCampaignPlayers = mapFromList currentPlayers
                                }
                        }
                    )
              else pure c
          _ | s `elem` theWebOfDreamsSteps -> do
            if currentCampaignMode meta == Just TheDreamQuest
              then do
                investigators <- allInvestigators
                currentPlayers <- for investigators \i -> do
                  player <- getPlayer i
                  iattrs <- getAttrs @Investigator i
                  pure (player, iattrs)
                if s == WakingNightmare && BeyondTheGatesOfSleep `elem` campaignCompletedSteps attrs
                  then do
                    players <- allPlayers
                    pushAll
                      $ ChoosingDecks
                      : map (\pid -> Msg.questionLabel "Choose Deck For Part B" pid ChooseDeck) players
                        <> [DoneChoosingDecks]
                  else do
                    for_ (mapToList $ otherCampaignPlayers meta) \(pid, iattrs) -> do
                      let i = overAttrs (const iattrs) $ lookupInvestigator (toId iattrs) pid
                      push $ SetInvestigator pid i
                let newAttrs = fromJustNote "not full campaign" (otherCampaignAttrs meta)
                pure
                  $ TheDreamEaters
                    ( newAttrs
                        { campaignCompletedSteps = campaignCompletedSteps attrs
                        , campaignStep = s
                        , campaignMeta =
                            toJSON
                              $ meta
                                { currentCampaignMode = Just TheWebOfDreams
                                , otherCampaignAttrs = Just $ attrs {campaignMeta = Null}
                                , currentCampaignPlayers = otherCampaignPlayers meta
                                , otherCampaignPlayers = mapFromList currentPlayers
                                }
                        }
                    )
              else pure c
          _ -> error $ "Unknown scenario: " <> show s
        lift $ defaultCampaignRunner msg c'
      CampaignStep (InterludeStep 1 _) -> do
        case campaignMode meta of
          PartialMode TheWebOfDreams -> push $ CampaignStep (InterludeStepPart 1 Nothing 3)
          _ -> push $ CampaignStep (InterludeStepPart 1 Nothing 1)
        pure c
      CampaignStep (InterludeStepPart 1 _ 1) -> do
        story theBlackCat1
        push $ case campaignMode meta of
          FullMode -> CampaignStep (InterludeStepPart 1 Nothing 2)
          _ -> NextCampaignStep (Just TheSearchForKadath)
        pure c
      CampaignStep (InterludeStepPart 1 _ 2) -> do
        storyWithChooseOne
          theBlackCat2
          [ Label
              "Tell your companions of your quest, your plight, and your peril. The black cat will return to you once this message is delivered. This may put an undue burden on your companions. "
              [InTheDreamQuest (Record TheBlackCatDeliveredNewsOfYourPlight)]
          , Label
              "Tell your companions about your new friends and about the Dreamlands."
              [InTheDreamQuest (Record TheBlackCatSharedKnowledgeOfTheDreamlands)]
          , Label
              "Tell your companions that they are in danger, and that you are safe. The black cat will stay with them once this message is delivered. This might make your quest a little more difficult."
              [InTheDreamQuest (Record TheBlackCatWarnedTheOthers)]
          , Label
              "You don’t trust this creature one bit. You threaten the black cat, warning it not to approach your friends under any circumstance. The black cat yawns and vanishes out the door."
              [InTheDreamQuest (Record OkayFineHaveItYourWayThen)]
          ]
        push $ CampaignStep (InterludeStepPart 1 Nothing 3)
        pure c
      CampaignStep (InterludeStepPart 1 _ 3) -> do
        lead <- getLeadPlayer
        let
          next =
            case campaignMode meta of
              FullMode ->
                Msg.questionLabel "Proceed to which scenario" lead
                  $ ChooseOne
                    [ Label "The Search for Kadath" [NextCampaignStep (Just TheSearchForKadath)]
                    , Label "A Thousand Shapes of Horror" [NextCampaignStep (Just AThousandShapesOfHorror)]
                    ]
              _ -> NextCampaignStep (Just AThousandShapesOfHorror)

        story theBlackCat3

        isOnYourOwn <- getIsTheWebOfDreams
        when isOnYourOwn do
          story youAreOnYourOwn
          record TheWebOfDreams YouAreOnYourOwn

        whenHasRecord TheBlackCatSharedKnowledgeOfTheDreamlands do
          story theBlackCatSharedKnowledgeOfTheDreamlands
          recordInBoth TheBlackCatHasAHunch

        whenHasRecord TheBlackCatDeliveredNewsOfYourPlight do
          story theBlackCatDeliveredNewsOfYourPlight
          pushAll
            [ InTheDreamQuest (Record TheBlackCatIsAtYourSide)
            , InTheDreamQuest (AddChaosToken ElderThing)
            , InTheWebOfDreams (AddChaosToken ElderThing)
            ]

        whenHasRecord TheBlackCatWarnedTheOthers do
          story theBlackCatWarnedTheOthers
          pushAll
            [ InTheWebOfDreams (Record TheBlackCatIsAtYourSide)
            , InTheWebOfDreams (AddChaosToken Tablet)
            , InTheDreamQuest (AddChaosToken Tablet)
            ]

        whenHasRecord OkayFineHaveItYourWayThen do
          story okayFineHaveItYourWayThen
          recordInBoth YouAskedForIt
        push next
        pure c
      CampaignStep (InterludeStep 2 _) -> do
        setCampaignPart TheWebOfDreams c (InterludeStepPart 2 Nothing 1)
      CampaignStep (InterludeStepPart 2 _ 1) -> do
        -- Start TheWebOfDreams
        story theOneironauts1

        hasAHunch <- getHasRecord TheWebOfDreams TheBlackCatHasAHunch
        randolphDidNotSurvive <- getHasRecord TheWebOfDreams RandolphDidNotSurviveTheDescent

        when (hasAHunch && randolphDidNotSurvive) do
          story where'sBlondie
          pushAll
            [ InTheDreamQuest (CrossOutRecord TheBlackCatHasAHunch)
            , InTheWebOfDreams (CrossOutRecord TheBlackCatHasAHunch)
            ]

        didYouAskForIt <- getHasRecord TheWebOfDreams YouAskedForIt
        if didYouAskForIt
          then do
            story youAskedForIt
            push $ CampaignStep (InterludeStepPart 2 Nothing 4)
            pure c
          else do
            storyWithChooseOne youDidNotAskForIt
              $ if hasAHunch
                then
                  [ Label
                      "The black cat wanders off before you can reply, its pitch-black fur melding into the darkness of the Underworld."
                      []
                  ]
                else
                  [ Label
                      "Tell your companions that you are in trouble. The black cat will return to you with aid once this message is delivered. This may put an undue burden on your companions."
                      [InTheWebOfDreams (Record TheBlackCatRequestedAidFromTheOthers)]
                  , Label
                      "Tell your companions about the Underworld. The black cat will then go elsewhere."
                      [InTheWebOfDreams (Record TheBlackCatSharedKnowledgeOfTheUnderworld)]
                  , Label
                      "Tell your companions that you are safe. The black cat will stay with them once this message is delivered. This might make your quest a little more difficult. "
                      [InTheWebOfDreams (Record TheBlackCatWarnedTheOthers)]
                  ]

            setCampaignPart TheDreamQuest c (InterludeStepPart 2 Nothing 2)
      CampaignStep (InterludeStepPart 2 _ 2) -> do
        -- TheDreamQuest
        story theOneironauts2
        notCaptured <- selectAny $ not_ (InvestigatorWithRecord WasCaptured)
        randolphEludedCapture <- getHasRecord TheDreamQuest RandolphEludedCapture

        story
          $ if notCaptured
            then atLeastOneNotCaptured
            else allCaptured

        hasAHunch <- getHasRecord TheDreamQuest TheBlackCatHasAHunch
        if hasAHunch && randolphEludedCapture
          then do
            story searchingForTheTruth
            recordInBoth TheBlackCatIsSearchingForTheTruth
            push $ CampaignStep (InterludeStepPart 2 Nothing 4)
            pure c
          else do
            setCampaignPart TheWebOfDreams c (InterludeStepPart 2 Nothing 3)
      CampaignStep (InterludeStepPart 2 _ 3) -> do
        story nowWhereWasI

        requestedAid <- getHasRecord TheWebOfDreams TheBlackCatRequestedAidFromTheOthers
        warnedTheOthers <- getHasRecord TheWebOfDreams TheBlackCatWarnedTheOthers
        sharedTheKnowledge <- getHasRecord TheWebOfDreams TheBlackCatSharedKnowledgeOfTheUnderworld
        theBlackCatIsAtYourSideWebOfDreams <- getHasRecord TheWebOfDreams TheBlackCatIsAtYourSide
        theBlackCatIsAtYourSideDreamQuest <- getHasRecord TheDreamQuest TheBlackCatIsAtYourSide

        when requestedAid do
          story theBlackCatRequestedAidFromTheOthers

          unless (theBlackCatIsAtYourSideWebOfDreams || theBlackCatIsAtYourSideDreamQuest) do
            record TheWebOfDreams TheBlackCatIsAtYourSide
            pushBoth $ AddChaosToken ElderThing

          when theBlackCatIsAtYourSideDreamQuest do
            push $ InTheDreamQuest (CrossOutRecord TheBlackCatIsAtYourSide)
            record TheWebOfDreams TheBlackCatIsAtYourSide
            pushBoth $ SwapChaosToken ElderThing Tablet

        when warnedTheOthers do
          story warnedTheOthersStory
          unless (theBlackCatIsAtYourSideWebOfDreams || theBlackCatIsAtYourSideDreamQuest) do
            record TheDreamQuest TheBlackCatIsAtYourSide
            pushBoth $ AddChaosToken ElderThing

          when theBlackCatIsAtYourSideWebOfDreams do
            push $ InTheWebOfDreams (CrossOutRecord TheBlackCatIsAtYourSide)
            record TheDreamQuest TheBlackCatIsAtYourSide
            pushBoth $ SwapChaosToken Tablet ElderThing

        when sharedTheKnowledge do
          story sharedTheKnowledgeStory
          record TheDreamQuest TheDreamersKnowOfAnotherPath

          when theBlackCatIsAtYourSideDreamQuest do
            push $ InTheDreamQuest (CrossOutRecord TheBlackCatIsAtYourSide)
            pushBoth $ RemoveChaosToken ElderThing

          when theBlackCatIsAtYourSideWebOfDreams do
            push $ InTheWebOfDreams (CrossOutRecord TheBlackCatIsAtYourSide)
            pushBoth $ RemoveChaosToken Tablet

        push $ CampaignStep (InterludeStepPart 2 Nothing 4)
        pure c
      CampaignStep (InterludeStepPart 2 _ 4) -> do
        lead <- getLeadPlayer
        push
          $ Msg.questionLabel "Proceed to which scenario" lead
          $ ChooseOne
            [ Label "Dark Side of the Moon" [NextCampaignStep (Just DarkSideOfTheMoon)]
            , Label "Point of No Return" [NextCampaignStep (Just PointOfNoReturn)]
            ]
        pure c
      CampaignStep (InterludeStep 3 _) -> do
        setCampaignPart TheDreamQuest c (InterludeStepPart 3 Nothing 1)
      CampaignStep (InterludeStepPart 3 _ 1) -> do
        story theGreatOnes1
        whenM (getHasRecord TheDreamQuest TheDreamersGrowWeaker) do
          story theGreatOnes1GrowWeaker
          addChaosToken $ case attrs.difficulty of
            Easy -> MinusThree
            Standard -> MinusFour
            Hard -> MinusFive
            Expert -> MinusSeven

        randolphDidNotSurvive <- getHasRecord TheDreamQuest RandolphCarterDidNotSurviveTheVoyage
        isSearchingForTheTruth <- getHasRecord TheDreamQuest TheBlackCatIsSearchingForTheTruth

        when (randolphDidNotSurvive && isSearchingForTheTruth) do
          story theGreatOnes1Searching
          pushAll
            [ InTheDreamQuest (CrossOutRecord TheBlackCatIsSearchingForTheTruth)
            , InTheWebOfDreams (CrossOutRecord TheBlackCatIsSearchingForTheTruth)
            ]

        didYouAskForIt <- getHasRecord TheDreamQuest YouAskedForIt
        if didYouAskForIt
          then do
            story theGreatOnes1YouAskedForIt
            push $ CampaignStep (InterludeStepPart 3 Nothing 3)
            pure c
          else do
            hasAHunch <- getHasRecord TheDreamQuest TheBlackCatHasAHunch
            storyWithChooseOne theGreatOnes1Part2
              $ if hasAHunch
                then
                  [ Label
                      "The black cat bounds off into the void of space before you get the chance to ask it anything else."
                      []
                  ]
                else
                  [ Label
                      "Tell your companions about the threats that you face. The black cat will return to you with aid once this message is delivered. This may put an undue burden on your companions. "
                      [InTheDreamQuest (Record TheBlackCatSpokeOfNyarlathotep)]
                  , Label
                      "Tell your companions that you will be okay. The black cat will stay with them once this message is delivered. This might make your quest a little more difficult. "
                      [InTheDreamQuest (Record TheBlackCatSpokeOfAtlachNacha)]
                  ]
            setCampaignPart TheWebOfDreams c (InterludeStepPart 3 Nothing 2)
      CampaignStep (InterludeStepPart 3 _ 2) -> do
        story theGreatOnes2
        possessTheSilverKey <- getHasRecord TheWebOfDreams TheInvestigatorsPossessTheSilverKey

        if possessTheSilverKey
          then do
            story theGreatOnes2TheSilverKey
            pushAll
              [ InTheWebOfDreams (CrossOutRecord TheInvestigatorsPossessTheSilverKey)
              , InTheDreamQuest (Record TheInvestigatorsPossessTheSilverKey)
              ]
            removeCampaignCard Assets.theSilverKey
            setCampaignPart TheDreamQuest c (InterludeStepPart 3 Nothing 21)
          else do
            push $ CampaignStep (InterludeStepPart 3 Nothing 22)
            pure c
      CampaignStep (InterludeStepPart 3 _ 21) -> do
        investigators <- allInvestigators
        addCampaignCardToDeckChoice investigators Assets.theSilverKey
        setCampaignPart TheDreamQuest c (InterludeStepPart 3 Nothing 22)
      CampaignStep (InterludeStepPart 3 _ 22) -> do
        -- If the black cat is searching for the truth:
        isSearching <- getHasRecord TheDreamQuest TheBlackCatIsSearchingForTheTruth
        if isSearching
          then do
            story theGreatOnes2Searching
            recordInBoth TheBlackCatKnowsTheTruth
          else do
            story theGreatOnes2Part2
            spokeOfNyarlathotep <- getHasRecord TheDreamQuest TheBlackCatSpokeOfNyarlathotep
            atYourSideTheWebOfDreams <- getHasRecord TheWebOfDreams TheBlackCatIsAtYourSide
            atYourSideTheDreamQuest <- getHasRecord TheDreamQuest TheBlackCatIsAtYourSide
            let neitherCampaignHasBlackCatAtYourSide = not atYourSideTheWebOfDreams && not atYourSideTheDreamQuest

            when spokeOfNyarlathotep do
              story theGreatOnes2Nyarlathotep

              if
                | neitherCampaignHasBlackCatAtYourSide -> do
                    record TheDreamQuest TheBlackCatIsAtYourSide
                    pushBoth $ AddChaosToken ElderThing
                | atYourSideTheWebOfDreams -> do
                    push $ InTheWebOfDreams (CrossOutRecord TheBlackCatIsAtYourSide)
                    push $ InTheDreamQuest (Record TheBlackCatIsAtYourSide)
                    pushBoth $ SwapChaosToken Tablet ElderThing
                | otherwise -> pure ()

            spokeOfAtlachNacha <- getHasRecord TheDreamQuest TheBlackCatSpokeOfAtlachNacha
            when spokeOfAtlachNacha do
              story theGreatOnes2AtlachNacha
              if
                | neitherCampaignHasBlackCatAtYourSide -> do
                    record TheWebOfDreams TheBlackCatIsAtYourSide
                    pushBoth $ AddChaosToken Tablet
                | atYourSideTheDreamQuest -> do
                    push $ InTheDreamQuest (CrossOutRecord TheBlackCatIsAtYourSide)
                    push $ InTheWebOfDreams (Record TheBlackCatIsAtYourSide)
                    pushBoth $ SwapChaosToken ElderThing Tablet
                | otherwise -> pure ()

        push $ CampaignStep (InterludeStepPart 3 Nothing 3)
        pure c
      CampaignStep (InterludeStepPart 3 _ 3) -> do
        lead <- getLeadPlayer
        push
          $ Msg.questionLabel "Proceed to which scenario" lead
          $ ChooseOne
            [ Label "Where the Gods Dwell" [NextCampaignStep (Just WhereTheGodsDwell)]
            , Label "Weaver of the Cosmos" [NextCampaignStep (Just WeaverOfTheCosmos)]
            ]
        pure c
      CampaignStep EpilogueStep -> do
        invasionHasBegun <- getHasRecord TheDreamQuest Nyarlathotep'sInvasionHasBegun
        awoke <- getHasRecord TheDreamQuest TheDreamersAwoke
        stayed <- getHasRecord TheDreamQuest TheDreamersStayedInTheDreamlandsForever
        traveled <- getHasRecord TheDreamQuest TheDreamersTraveledBeneathTheMonastery
        bridgeCompleted <- getHasRecord TheWebOfDreams TheBridgeWasCompleted
        returned <- getHasRecord TheWebOfDreams TheInvestigatorsReturnedToReality
        neverEscaped <- getHasRecord TheWebOfDreams TheInvestigatorsNeverEscaped
        stillInDreamlands <- getHasRecord TheWebOfDreams TheInvestigatorsAreStillInTheDreamlands
        if
          | invasionHasBegun ->
              if
                | bridgeCompleted -> do
                    story $ i18n "theDreamEaters.epilogue1"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | returned -> do
                    story $ i18n "theDreamEaters.epilogue2"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | neverEscaped -> do
                    story $ i18n "theDreamEaters.epilogue3"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | stillInDreamlands -> do
                    story $ i18n "theDreamEaters.epilogue4"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | otherwise -> error "invalid"
          | awoke ->
              if
                | bridgeCompleted -> do
                    story $ i18n "theDreamEaters.epilogue5"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 5)
                | returned -> do
                    story $ i18n "theDreamEaters.epilogue6"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | neverEscaped -> do
                    story $ i18n "theDreamEaters.epilogue7"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | stillInDreamlands -> do
                    story $ i18n "theDreamEaters.epilogue8"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | otherwise -> error "invalid"
          | stayed ->
              if
                | bridgeCompleted -> do
                    story $ i18n "theDreamEaters.epilogue9"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | returned -> do
                    story $ i18n "theDreamEaters.epilogue10"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | neverEscaped -> do
                    story $ i18n "theDreamEaters.epilogue11"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | stillInDreamlands -> do
                    story $ i18n "theDreamEaters.epilogue12"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | otherwise -> error "invalid"
          | traveled ->
              if
                | bridgeCompleted -> do
                    story $ i18n "theDreamEaters.epilogue13"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 13)
                | returned -> do
                    story $ i18n "theDreamEaters.epilogue14"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | neverEscaped -> do
                    story $ i18n "theDreamEaters.epilogue15"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | stillInDreamlands -> do
                    story $ i18n "theDreamEaters.epilogue16"
                    setCampaignPart TheDreamQuest c (EpilogueStepPart 17)
                | otherwise -> error "invalid"
          | otherwise -> error "invalid"
      CampaignStep (EpilogueStepPart 5) -> do
        eachInvestigator (kill attrs)
        push $ CampaignStep (EpilogueStepPart 17)
        pure c
      CampaignStep (EpilogueStepPart 13) -> do
        eachInvestigator $ push . DrivenInsane
        push $ CampaignStep (EpilogueStepPart 17)
        pure c
      CampaignStep (EpilogueStepPart 17) -> do
        iids <- select $ InvestigatorWithRecord WasCaptured <> not_ DefeatedInvestigator
        when (notNull iids) do
          players <- traverse getPlayer iids
          push $ Msg.story players $ i18n "theDreamEaters.brokeTheLawOfUlthar"
          for_ iids $ kill attrs
        push GameOver
        pure c
      InTheDreamQuest msg' -> do
        case currentCampaignMode meta of
          Nothing -> error "called with no campaign mode"
          Just TheDreamQuest -> lift (defaultCampaignRunner msg' c)
          Just TheWebOfDreams -> case otherCampaignAttrs meta of
            Nothing -> error "called with no other campaign attrs"
            Just otherAttrs -> do
              TheDreamEaters otherAttrs' <- lift (defaultCampaignRunner msg' (TheDreamEaters otherAttrs))
              pure
                $ TheDreamEaters
                $ attrs {campaignMeta = toJSON $ meta {otherCampaignAttrs = Just otherAttrs'}}
      InTheWebOfDreams msg' -> do
        case currentCampaignMode meta of
          Nothing -> error "called with no campaign mode"
          Just TheWebOfDreams -> lift (defaultCampaignRunner msg' c)
          Just TheDreamQuest -> case otherCampaignAttrs meta of
            Nothing -> error "called with no other campaign attrs"
            Just otherAttrs -> do
              TheDreamEaters otherAttrs' <- lift (defaultCampaignRunner msg' (TheDreamEaters otherAttrs))
              pure
                $ TheDreamEaters
                $ attrs {campaignMeta = toJSON $ meta {otherCampaignAttrs = Just otherAttrs'}}
      _ -> lift (defaultCampaignRunner msg c)
