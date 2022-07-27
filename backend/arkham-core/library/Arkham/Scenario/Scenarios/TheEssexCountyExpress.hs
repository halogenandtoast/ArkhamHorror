module Arkham.Scenario.Scenarios.TheEssexCountyExpress
  ( TheEssexCountyExpress(..)
  , theEssexCountyExpress
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Direction
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Agenda
import Arkham.Helpers.Card
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheEssexCountyExpress = TheEssexCountyExpress ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theEssexCountyExpress :: Difficulty -> TheEssexCountyExpress
theEssexCountyExpress difficulty = scenario
  TheEssexCountyExpress
  "02159"
  "The Essex County Express"
  difficulty
  ["trainCar6 trainCar5 trainCar4 trainCar3 trainCar2 trainCar1 engineCar"]

theEssexCountyExpressIntro :: Message
theEssexCountyExpressIntro = FlavorText
  (Just "Scenario III: The Essex County Express")
  [ "Recent events in the Museum have forced you to\
    \ re-evaluate Armitage’s tale about Dunwich. It\
    \ cannot be a coincidence—Wilbur Whateley, the\
    \ Necronomicon, the creature from Dunwich, and\
    \ the people and creatures who attacked here in\
    \ Arkham—everything must be connected. You’re\
    \ certain now where you must head: the lonely\
    \ and dismal town of Dunwich Village."
  , "You consider telling the Massachusetts State Police what you know, but\
    \ the negative consequences outweigh the potential gain. Given the nature\
    \ of your story, they would likely write your stories off as an absurd hoax.\
    \ Worse, they could lock you up. After all, you were present in an illegal\
    \ speakeasy, and you also trespassed in the museum. Instead, you decide\
    \ to head to Dunwich yourself, in order to investigate further."
  , "You pack everything you think you might need and manage to get\
    \ some rest for the night. In the morning, you head to the train station in\
    \ Northside and purchase a last-minute express ticket. Dunwich is several\
    \ hours by train northwest along the Miskatonic River Valley. There is no\
    \ train station in Dunwich, but you manage to phone one of Armitage’s\
    \ acquaintances in the small village: a man by the name of Zebulon\
    \ Whateley who was present during the events several months ago."
  , "Armitage’s notes indicate that the Whateley family is spread across\
    \ many branches, some decadent and unscrupulous, others “undecayed”\
    \ or otherwise untouched by nefarious and diabolic rites. According to\
    \ Armitage, Zebulon’s branch of the family lay somewhere between the\
    \ decayed and undecayed Whateleys, who knew of the traditions of his\
    \ ancestors, but was not corrupted by them. He agrees to pick you up at\
    \ the closest station and drive you into town."
  , "As the train departs from Arkham, you feel the events of the previous\
    \ night catching up to you, and exhaustion sets in. But before you can\
    \ safely reach your destination, the train car suddenly rumbles and\
    \ shakes, startling you out of your reverie. The train loudly skids to a\
    \ violent halt, and you hear a rattling noise behind you…"
  ]

instance HasTokenValue TheEssexCountyExpress where
  getTokenValue iid tokenFace (TheEssexCountyExpress attrs) = case tokenFace of
    Skull -> do
      step <- getCurrentAgendaStep
      pure $ toTokenValue attrs Skull step (step + 1)
    Cultist -> pure $ toTokenValue attrs Cultist 1 0
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 3
    otherFace -> getTokenValue iid otherFace attrs

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
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

investigatorDefeat :: (Monad m, HasGame m) => m [Message]
investigatorDefeat = do
  campaignStoryCards <- getCampaignStoryCards
  leadInvestigatorId <- getLeadInvestigatorId
  defeatedInvestigatorIds <- selectList DefeatedInvestigator
  let
    findOwner cardCode =
      (\iid -> iid <$ guard (iid `elem` defeatedInvestigatorIds))
        =<< findKey (any ((== cardCode) . toCardCode)) campaignStoryCards
    mNecronomiconOwner = findOwner "02140"
    mDrHenryArmitageOwner = findOwner "02040"
    mProfessorWarrenRiceOwner = findOwner "02061"
    mDrFrancisMorganOwner = findOwner "02080"
  if null defeatedInvestigatorIds
    then pure []
    else
      pure
      $ [ chooseOne
            leadInvestigatorId
            [ Run
                [ Continue "Continue"
                , FlavorText
                  (Just "Investigator Defeat")
                  [ "Your experience beyond the gate is\
                   \ simultaneously terrifying and impossible to recall with clarity.\
                   \ A hypnotic spectacle of lights, otherworldly sensations, and\
                   \ altered geometry dances at the tattered edges of your mind. An\
                   \ unearthly voice from beyond rings in your ears, its significance\
                   \ an enigma. When you awaken, you find yourself in the woods,\
                   \ several miles from the Miskatonic River. Destroyed train cars\
                   \ surround you. They are crumpled as if from a severe impact;\
                   \ they are also decayed as if years of rust and squalor have\
                   \ claimed them. There is no sign of the other passengers."
                  ]
                ]
            ]
        ]
      <> [ Record TheNecronomiconWasStolen | isJust mNecronomiconOwner ]
      <> [ RemoveCampaignCardFromDeck owner "02140"
         | owner <- maybeToList mNecronomiconOwner
         ]
      <> [ Record DrHenryArmitageWasKidnapped | isJust mDrHenryArmitageOwner ]
      <> [ RemoveCampaignCardFromDeck owner "02040"
         | owner <- maybeToList mDrHenryArmitageOwner
         ]
      <> [ Record ProfessorWarrenRiceWasKidnapped
         | isJust mProfessorWarrenRiceOwner
         ]
      <> [ RemoveCampaignCardFromDeck owner "02061"
         | owner <- maybeToList mProfessorWarrenRiceOwner
         ]
      <> [ Record DrFrancisMorganWasKidnapped | isJust mDrFrancisMorganOwner ]
      <> [ RemoveCampaignCardFromDeck owner "02080"
         | owner <- maybeToList mDrFrancisMorganOwner
         ]
      <> [ AddCampaignCardToDeck iid Treacheries.acrossSpaceAndTime
         | iid <- defeatedInvestigatorIds
         ]

instance RunMessage TheEssexCountyExpress where
  runMessage msg s@(TheEssexCountyExpress attrs@ScenarioAttrs {..}) =
    case msg of
      SetTokensForScenario -> do
        standalone <- getIsStandalone
        s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
      Setup -> do
        investigatorIds <- getInvestigatorIds

        let toLocationCard = fmap EncounterCard . genEncounterCard

        engineCar <- toLocationCard =<< sample
          (Locations.engineCar_175
          :| [Locations.engineCar_176, Locations.engineCar_177]
          )
        let engineCarId = LocationId $ toCardId engineCar

        trainCars <- traverse toLocationCard . take 6 =<< shuffleM
          [ Locations.passengerCar_167
          , Locations.passengerCar_168
          , Locations.passengerCar_169
          , Locations.passengerCar_170
          , Locations.passengerCar_171
          , Locations.sleepingCar
          , Locations.diningCar
          , Locations.parlorCar
          ]

        encounterDeck <- buildEncounterDeck
          [ EncounterSet.TheEssexCountyExpress
          , EncounterSet.TheBeyond
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.DarkCult
          ]

        let
          start =
            LocationId . toCardId . fromJustNote "No train cars?" $ headMay
              trainCars
          end = LocationId . toCardId . fromJustNote "No train cars?" $ headMay
            (reverse trainCars)
          allCars = trainCars <> [engineCar]
          token = case scenarioDifficulty of
            Easy -> MinusTwo
            Standard -> MinusThree
            Hard -> MinusFour
            Expert -> MinusFive

        pushAll
          $ [ story investigatorIds theEssexCountyExpressIntro
            , AddToken token
            , SetEncounterDeck encounterDeck
            , SetAgendaDeck
            , SetActDeck
            ]
          <> concat
               [ [ PlaceLocation card
                 , SetLocationLabel
                   (LocationId $ toCardId card)
                   ("trainCar" <> tshow @Int n)
                 ]
               | (n, card) <- zip [6, 5 ..] trainCars
               ]
          <> [ PlacedLocationDirection
                 (LocationId $ toCardId l1)
                 LeftOf
                 (LocationId $ toCardId l2)
             | (l1, l2) <- zip allCars (drop 1 allCars)
             ]
          <> [ PlaceLocation engineCar
             , PlacedLocationDirection engineCarId RightOf end
             , CreateWindowModifierEffect
               EffectSetupWindow
               (EffectModifiers [Modifier (ScenarioSource scenarioId) Blank])
               (ScenarioSource scenarioId)
               (LocationTarget start)
             , RevealLocation Nothing start
             , MoveAllTo (toSource attrs) start
             ]

        setAsideCards <- traverse
          genCard
          [ Treacheries.acrossSpaceAndTime
          , Treacheries.acrossSpaceAndTime
          , Treacheries.acrossSpaceAndTime
          , Treacheries.acrossSpaceAndTime
          ]

        TheEssexCountyExpress <$> runMessage
          msg
          (attrs
          & (setAsideCardsL .~ setAsideCards)
          & (actStackL . at 1 ?~ [Acts.run, Acts.getTheEngineRunning])
          & (agendaStackL
            . at 1
            ?~ [ Agendas.aTearInReality
               , Agendas.theMawWidens
               , Agendas.rollingBackwards
               , Agendas.drawnIn
               , Agendas.outOfTime
               ]
            )
          )
      ResolveToken _ Tablet iid | isEasyStandard attrs -> do
        closestCultists <- selectList $ NearestEnemyTo iid $ EnemyWithTrait
          Trait.Cultist
        s <$ case closestCultists of
          [] -> pure ()
          [x] -> push (PlaceDoom (EnemyTarget x) 1)
          xs -> push (chooseOne iid [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
      ResolveToken _ Tablet _ | isHardExpert attrs -> do
        cultists <- selectList $ EnemyWithTrait Trait.Cultist
        s <$ pushAll [ PlaceDoom (EnemyTarget eid) 1 | eid <- cultists ]
      FailedSkillTest iid _ _ (TokenTarget token) _ n ->
        s <$ case tokenFace token of
          Cultist ->
            pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
          ElderThing | isEasyStandard attrs -> push $ ChooseAndDiscardCard iid
          ElderThing | isHardExpert attrs ->
            pushAll $ replicate n (ChooseAndDiscardCard iid)
          _ -> pure ()
      ScenarioResolution NoResolution ->
        s <$ pushAll [ScenarioResolution $ Resolution 2]
      ScenarioResolution (Resolution 1) -> do
        msgs <- investigatorDefeat
        leadInvestigatorId <- getLeadInvestigatorId
        defeatedInvestigatorIds <- selectList DefeatedInvestigator
        xp <- getXp
        s <$ pushAll
          (msgs
          <> [ chooseOne
                 leadInvestigatorId
                 [ Run
                     [ Continue "Continue"
                     , FlavorText
                       (Just "Resolution 1")
                       [ "You breathe a sigh of relief as the gate behind\
                     \ the train collapses harmlessly upon itself. The few passengers\
                     \ who survived the ordeal seem unable to comprehend what\
                     \ just happened. One passenger mentions “a pipe bursting in\
                     \ the rear car,” and that quickly becomes the explanation for\
                     \ the innocent and ignorant, those who either cannot or choose\
                     \ not to delve further into the mystery. You, on the other hand,\
                     \ know better… although in hindsight, you wish you didn’t."
                       ]
                     ]
                 ]
             ]
          <> [ GainXP
                 iid
                 (n + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | (iid, n) <- xp
             ]
          <> [EndOfGame Nothing]
          )
      ScenarioResolution (Resolution 2) -> do
        msgs <- investigatorDefeat
        leadInvestigatorId <- getLeadInvestigatorId
        defeatedInvestigatorIds <- selectList DefeatedInvestigator
        xp <- getXp
        s <$ pushAll
          (msgs
          <> [ chooseOne
               leadInvestigatorId
               [ Run
                   [ Continue "Continue"
                   , FlavorText
                     (Just "Resolution 2")
                     [ "Rattled,\
                     \ you begin walking alongside the train tracks, making your\
                     \ way towards Dunwich."
                     ]
                   ]
               ]
             , Record TheInvestigatorsWereDelayedOnTheirWayToDunwich
             ]
          <> [ GainXP
                 iid
                 (n + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | (iid, n) <- xp
             ]
          <> [EndOfGame Nothing]
          )
      _ -> TheEssexCountyExpress <$> runMessage msg attrs
