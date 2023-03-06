module Arkham.Scenario.Scenarios.CurseOfTheRougarou
  ( CurseOfTheRougarou(..)
  , curseOfTheRougarou
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.CurseOfTheRougarou.FlavorText
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding ( Cultist )
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Maybe ( fromJust )

newtype CurseOfTheRougarou = CurseOfTheRougarou ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

curseOfTheRougarou :: Difficulty -> CurseOfTheRougarou
curseOfTheRougarou difficulty = scenario
  CurseOfTheRougarou
  "81001"
  "Curse of the Rougarou"
  difficulty
  [ "     .       unhallowed1      newOrleans1       ."
  , "unhallowed2 unhallowedBayou newOrleansBayou newOrleans2"
  , "riverside2 riversideBayou wildernessBayou wilderness2"
  , "     .       riverside1      wilderness1       ."
  ]

instance HasTokenValue CurseOfTheRougarou where
  getTokenValue iid tokenFace (CurseOfTheRougarou attrs) = case tokenFace of
    Skull -> do
      isBayou <- selectAny $ LocationWithTrait Bayou <> LocationWithInvestigator
        (InvestigatorWithId iid)
      pure $ if isBayou
        then toTokenValue attrs Skull 4 6
        else toTokenValue attrs Skull 2 3
    Cultist -> pure $ toTokenValue attrs Cultist 2 3
    Tablet -> pure $ TokenValue Tablet ZeroModifier
    ElderThing -> pure $ TokenValue ElderThing (NegativeModifier 4)
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage CurseOfTheRougarou where
  runMessage msg s@(CurseOfTheRougarou attrs) = case msg of
    Setup -> do
      investigatorIds <- allInvestigatorIds
      encounterDeck <- buildEncounterDeck [EncounterSet.TheBayou]
      result <- shuffleM $ keys locationsByTrait
      let
        trait = fromJust . headMay . drop 1 $ result
        rest = drop 2 result

      startingLocations <- traverse genCard
        $ findWithDefault [] trait locationsByTrait
      startingLocationsWithLabel <- locationsWithLabels trait startingLocations

      setAsideLocations <- traverse genCard
        $ concatMap (\t -> findWithDefault [] t locationsByTrait) rest

      setAsideCards <- traverse
        genCard
        [Assets.ladyEsprit, Assets.bearTrap, Assets.fishingNet]

      let
        ((bayouLabel, bayou), others) =
          case break (elem Bayou . toTraits . snd) startingLocationsWithLabel of
            (as, x : bs) -> (x, as <> bs)
            _ -> error "handled"

      (bayouId, placeBayou) <- placeLocation bayou
      otherPlacements <- for others $ \(label, card) -> do
        (locationId, placement) <- placeLocation card
        pure [placement, SetLocationLabel locationId label]

      pushAll
        $ [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck]
        <> [placeBayou, SetLocationLabel bayouId bayouLabel]
        <> concat otherPlacements
        <> [ RevealLocation Nothing bayouId
           , MoveAllTo (toSource attrs) bayouId
           , story investigatorIds intro
           ]

      CurseOfTheRougarou <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideLocations <> setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.findingLadyEsprit, Acts.huntingTheRougarou]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.aCreatureOfTheBayou
             , Agendas.theRougarouFeeds
             , Agendas.theCurseSpreads
             ]
          )
        )
    SetTokensForScenario -> do
      let
        tokens = if isEasyStandard attrs
          then
            [ PlusOne
            , PlusOne
            , Zero
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
            , MinusFour
            , MinusFive
            , MinusSix
            , Skull
            , Skull
            , Cultist
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
          else
            [ PlusOne
            , Zero
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
            , MinusFour
            , MinusFive
            , MinusFive
            , MinusSix
            , MinusEight
            , Skull
            , Skull
            , Skull
            , Cultist
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
      s <$ push (SetTokens tokens)
    ResolveToken _ Cultist iid -> do
      rougarouAtYourLocation <-
        selectAny $ enemyIs Enemies.theRougarou <> EnemyAt
          (LocationWithInvestigator $ InvestigatorWithId iid)
      s <$ when rougarouAtYourLocation (push $ DrawAnotherToken iid)
    ResolveToken _ ElderThing iid -> if isEasyStandard attrs
      then do
        mrougarou <- selectOne $ enemyIs Enemies.theRougarou <> EnemyAt
          (LocationWithInvestigator $ InvestigatorWithId iid)
        s <$ for_
          mrougarou
          (\eid -> push $ EnemyWillAttack $ enemyAttack eid iid)
      else do
        lid <- getJustLocation iid
        connectedLocationIds <- selectList $ AccessibleFrom $ LocationWithId lid
        mrougarou <-
          selectOne $ enemyIs Enemies.theRougarou <> EnemyAt
            (LocationMatchAny $ map LocationWithId $ lid : connectedLocationIds)
        s <$ for_
          mrougarou
          (\eid -> push $ EnemyWillAttack $ enemyAttack eid iid)
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> s <$ when
      (tokenFace token == Tablet)
      (push $ CreateEffect
        "81001"
        Nothing
        (TokenSource token)
        (InvestigatorTarget iid)
      )
    ScenarioResolution NoResolution ->
      runMessage (ScenarioResolution $ Resolution 1) s
    ScenarioResolution (Resolution 1) -> do
      iids <- allInvestigatorIds
      xp <- getXp
      pushAll
        $ [story iids resolution1, Record TheRougarouContinuesToHauntTheBayou]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      iids <- allInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      pushAll
        $ [ story iids resolution2
          , Record TheRougarouIsDestroyed
          , RemoveCampaignCard Treacheries.curseOfTheRougarou
          , chooseOne
            leadInvestigatorId
            [ Label
              "Add Lady Esprit to your deck"
              [AddCampaignCardToDeck leadInvestigatorId Assets.ladyEsprit]
            , Label "Do not add Lady Esprit to your deck" []
            ]
          ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 3) -> do
      iids <- allInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      pushAll
        $ [ story iids resolution3
          , Record TheRougarouEscapedAndYouEmbracedTheCurse
          , AddCampaignCardToDeck
            leadInvestigatorId
            Assets.monstrousTransformation
          ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    _ -> CurseOfTheRougarou <$> runMessage msg attrs
