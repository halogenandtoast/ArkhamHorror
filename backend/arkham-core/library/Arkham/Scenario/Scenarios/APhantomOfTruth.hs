module Arkham.Scenario.Scenarios.APhantomOfTruth (
  APhantomOfTruth (..),
  aPhantomOfTruth,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.APhantomOfTruth.Story
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.Scenario.Attrs
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait (Trait(Byakhee))

newtype APhantomOfTruth = APhantomOfTruth ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aPhantomOfTruth :: Difficulty -> APhantomOfTruth
aPhantomOfTruth difficulty =
  APhantomOfTruth $
    baseAttrs "03200" "A Phantom of Truth" difficulty
      & locationLayoutL
      ?~ [ "grandGuignol .                   canalSaintMartin ."
         , "grandGuignol montmartre           canalSaintMartin péreLachaiseCemetery"
         , "opéraGarnier montmartre           leMarais         péreLachaiseCemetery"
         , "opéraGarnier .                   leMarais         ."
         , "gareDOrsay   gardensOfLuxembourg notreDame        ."
         , "gareDOrsay   gardensOfLuxembourg notreDame        ."
         , ".            montparnasse        .                ."
         , ".            montparnasse        .                ."
         ]

instance HasRecord env APhantomOfTruth where
  hasRecord _ _ = pure False
  hasRecordSet _ _ = pure []
  hasRecordCount _ _ = pure 0

instance (HasCount DoomCount env (), HasTokenValue env InvestigatorId) => HasTokenValue env APhantomOfTruth where
  getTokenValue (APhantomOfTruth attrs) iid = \case
    Skull -> do
      doom <- unDoomCount <$> getCount ()
      pure $ toTokenValue attrs Skull (min 5 doom) doom
    Cultist -> pure $ TokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 2 3
    otherFace -> getTokenValue attrs iid otherFace

standaloneTokens :: [TokenFace]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

gatherTheMidnightMasks :: MonadRandom m => Int -> Int -> m [EncounterCard]
gatherTheMidnightMasks conviction doubt =
  traverse
    genEncounterCard
    ( if conviction > doubt
        then
          [ Treacheries.huntingShadow
          , Treacheries.huntingShadow
          , Treacheries.huntingShadow
          ]
        else [Treacheries.falseLead, Treacheries.falseLead]
    )

cultistEffect ::
  forall m env.
  ( HasList (InvestigatorId, Distance) env EnemyMatcher
  , MonadReader env m
  , HasId LeadInvestigatorId env ()
  , Query EnemyMatcher env
  , HasQueue env
  , MonadIO m
  ) =>
  m ()
cultistEffect = do
  leadInvestigatorId <- getLeadInvestigatorId
  byakhee <- selectList (EnemyWithTrait Byakhee <> UnengagedEnemy)
  byakheePairs <- traverse (traverseToSnd investigatorsNearestToByakhee) byakhee
  push $
    chooseOneAtATime
      leadInvestigatorId
      [ TargetLabel
        (EnemyTarget eid)
        (moveTowardMessages leadInvestigatorId eid is)
      | (eid, is) <- byakheePairs
      ]
 where
  investigatorsNearestToByakhee :: EnemyId -> m (HashSet InvestigatorId)
  investigatorsNearestToByakhee eid = do
    mappings :: [(InvestigatorId, Distance)] <- getList (EnemyWithId eid)
    let minDistance :: Int =
          fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
    pure . setFromList . map fst $
      filter
        ((== minDistance) . unDistance . snd)
        mappings
  moveTowardMessages leadId eid hset = case toList hset of
    [] -> []
    [x] -> [moveToward eid x]
    xs ->
      [ chooseOne
          leadId
          [TargetLabel (InvestigatorTarget x) [moveToward eid x] | x <- xs]
      ]
  moveToward eid x =
    MoveToward
      (EnemyTarget eid)
      (LocationWithInvestigator $ InvestigatorWithId x)

instance ScenarioRunner env => RunMessage env APhantomOfTruth where
  runMessage msg s@(APhantomOfTruth attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ do
        randomToken <- sample (Cultist :| [Tablet, ElderThing])
        push (SetTokens $ standaloneTokens <> [randomToken, randomToken])
      pure s
    StandaloneSetup -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        ( chooseOne
            leadInvestigatorId
            [ Label "Conviction" [RecordCount Conviction 1]
            , Label "Doubt" [RecordCount Doubt 1]
            ]
        )
      pure s
    Setup -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId

      theKingClaimedItsVictims <- getHasRecord TheKingClaimedItsVictims
      youIntrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      youSlayedTheMonstersAtTheDinnerParty <-
        getHasRecord
          YouSlayedTheMonstersAtTheDinnerParty
      thePoliceAreSuspiciousOfYou <- getHasRecord ThePoliceAreSuspiciousOfYou
      chasingTheStranger <- getRecordCount ChasingTheStranger

      let showDream4 =
            not theKingClaimedItsVictims
              && not youIntrudedOnASecretMeeting
              && youSlayedTheMonstersAtTheDinnerParty
          showDream7 =
            not theKingClaimedItsVictims && thePoliceAreSuspiciousOfYou

      let intro = if theKingClaimedItsVictims then intro1 else intro2
          dreamPath =
            catMaybes
              [ Just dream1
              , Just dream2
              , dream3
                  <$ guard
                    (not theKingClaimedItsVictims && youIntrudedOnASecretMeeting)
              , dream4 <$ guard showDream4
              , dream6 <$ guard (not theKingClaimedItsVictims)
              , dream7 <$ guard showDream7
              , Just dream8
              , dream9 <$ guard (chasingTheStranger <= 3)
              , dream10 <$ guard (chasingTheStranger > 3)
              ]

      paranoia <- genPlayerCard Treacheries.paranoia
      lostSouls <- replicateM 4 (genPlayerCard Treacheries.lostSoul)
      standalone <- getIsStandalone

      pushAll $
        story investigatorIds intro :
        map (story investigatorIds) dreamPath
          <> [ ShuffleCardsIntoDeck iid [lostSoul]
             | not standalone
             , (iid, lostSoul) <- zip investigatorIds lostSouls
             ]
          <> [ chooseOne
              leadInvestigatorId
              [ TargetLabel
                (InvestigatorTarget iid)
                [ShuffleCardsIntoDeck iid [paranoia]]
              | iid <- investigatorIds
              ]
             | showDream4
             ]
          <> [SufferTrauma iid 0 1 | showDream7, iid <- investigatorIds]
          <> [ chooseOne
              leadInvestigatorId
              [ Label
                  "“How could any of this be beautiful to you?”"
                  [SetupStep 11]
              , Label "“What exactly am I looking at?”" [SetupStep 12]
              ]
             | chasingTheStranger > 3
             ]
          <> [SetupStep 13 | chasingTheStranger <= 3]
      APhantomOfTruth <$> runMessage msg attrs
    SetupStep n -> do
      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt

      let act1 =
            if conviction > doubt
              then Acts.theParisianConspiracyV2
              else Acts.theParisianConspiracyV1
          excludes =
            if conviction > doubt
              then [Treacheries.blackStarsRise]
              else [Treacheries.twinSuns]

      gatheredCards <-
        buildEncounterDeckExcluding
          excludes
          [ EncounterSet.APhantomOfTruth
          , EncounterSet.EvilPortents
          , EncounterSet.Byakhee
          , EncounterSet.TheStranger
          , EncounterSet.AgentsOfHastur
          ]
      midnightMasks <- gatherTheMidnightMasks conviction doubt
      encounterDeck <- Deck <$> shuffleM (unDeck gatheredCards <> midnightMasks)

      setAsideCards <- traverse genCard [Enemies.theOrganistHopelessIDefiedHim]

      montmartre <-
        genCard
          =<< sample (Locations.montmartre209 :| [Locations.montmartre210])

      operaGarnier <-
        genCard
          =<< sample (Locations.operaGarnier212 :| [Locations.operaGarnier213])

      leMarais <-
        genCard
          =<< sample (Locations.leMarais217 :| [Locations.leMarais218])

      montparnasse <- genCard Locations.montparnasse
      grandGuignol <- genCard Locations.grandGuignol
      gareDOrsay <- genCard Locations.gareDOrsay
      pereLachaiseCemetery <- genCard Locations.pereLachaiseCemetery
      canalSaintMartin <- genCard Locations.canalSaintMartin
      notreDame <- genCard Locations.notreDame
      gardensOfLuxembourg <- genCard Locations.gardensOfLuxembourg

      jordanInterviewed <-
        elem (Recorded $ toCardCode Assets.jordanPerry)
          <$> getRecordSet VIPsInterviewed
      investigatorIds <- getInvestigatorIds

      let startingLocation =
            if jordanInterviewed then montparnasse else gareDOrsay

      pushAll
        ( [story investigatorIds dream11 | n == 11]
          <> [story investigatorIds dream12 | n == 12]
            <> [RecordCount Doubt (doubt + 1) | n == 12]
            <> [story investigatorIds dream13, story investigatorIds awakening]
            <> [story investigatorIds jordansInformation | jordanInterviewed]
            <> [ TakeResources iid 3 False
               | jordanInterviewed
               , iid <- investigatorIds
               ]
            <> [ SetEncounterDeck encounterDeck
               , SetAgendaDeck
               , SetActDeck
               , PlaceLocation montmartre
               , PlaceLocation operaGarnier
               , PlaceLocation leMarais
               , PlaceLocation montparnasse
               , PlaceLocation gareDOrsay
               , PlaceLocation grandGuignol
               , PlaceLocation canalSaintMartin
               , PlaceLocation pereLachaiseCemetery
               , PlaceLocation notreDame
               , PlaceLocation gardensOfLuxembourg
               , MoveAllTo (toSource attrs) (toLocationId startingLocation)
               ]
        )

      APhantomOfTruth
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & ( actStackL
                    . at 1
                    ?~ [act1, Acts.pursuingShadows, Acts.stalkedByShadows]
                )
              & ( agendaStackL
                    . at 1
                    ?~ [ Agendas.theFirstNight
                       , Agendas.theSecondNight
                       , Agendas.theThirdNight
                       ]
                )
          )
    ResolveToken drawnToken tokenFace _ -> case tokenFace of
      Cultist | isHardExpert attrs -> s <$ cultistEffect
      Tablet -> s <$ pushAll
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [CancelSkills])
          (TokenSource drawnToken)
          SkillTestTarget
        , CancelSkillEffects
        ]
      _ -> pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ n -> case tokenFace token of
      Cultist | isEasyStandard attrs -> s <$ cultistEffect
      ElderThing -> s <$ push (LoseResources iid n)
      _ -> pure s
    _ -> APhantomOfTruth <$> runMessage msg attrs
