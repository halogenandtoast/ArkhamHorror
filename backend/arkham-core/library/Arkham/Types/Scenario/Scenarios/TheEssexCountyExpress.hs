module Arkham.Types.Scenario.Scenarios.TheEssexCountyExpress
  ( TheEssexCountyExpress(..)
  , theEssexCountyExpress
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Locations
import qualified Arkham.Treachery.Cards as Treacheries
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import qualified Arkham.Types.Trait as Trait

newtype TheEssexCountyExpress = TheEssexCountyExpress ScenarioAttrs
  deriving stock Generic
  deriving anyclass HasRecord
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theEssexCountyExpress :: Difficulty -> TheEssexCountyExpress
theEssexCountyExpress difficulty = TheEssexCountyExpress $ base
  { scenarioLocationLayout = Just
    ["trainCar6 trainCar5 trainCar4 trainCar3 trainCar2 trainCar1 engineCar"]
  }
 where
  base = baseAttrs
    "02159"
    "The Essex County Express"
    ["02160", "02161", "02162", "02163", "02164"]
    ["02165", "02166"]
    difficulty

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

instance
  ( HasTokenValue env InvestigatorId
  , HasStep env AgendaStep
  )
  => HasTokenValue env TheEssexCountyExpress where
  getTokenValue (TheEssexCountyExpress attrs) iid = \case
    Skull -> do
      step <- unAgendaStep <$> getStep
      pure $ toTokenValue attrs Skull step (step + 1)
    Cultist -> pure $ toTokenValue attrs Cultist 1 0
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 3
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
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

investigatorDefeat
  :: ( MonadReader env m
     , HasSet DefeatedInvestigatorId env ()
     , HasId LeadInvestigatorId env ()
     , HasList CampaignStoryCard env ()
     )
  => m [Message]
investigatorDefeat = do
  campaignStoryCards <- getList ()
  leadInvestigatorId <- getLeadInvestigatorId
  defeatedInvestigatorIds <- map unDefeatedInvestigatorId <$> getSetList ()
  let
    findOwner cardCode =
      (\iid -> iid <$ guard (iid `elem` defeatedInvestigatorIds))
        . campaignStoryCardInvestigatorId
        =<< find
              ((== cardCode) . toCardCode . campaignStoryCardPlayerCard)
              campaignStoryCards
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

instance (HasId (Maybe LocationId) env LocationMatcher, ScenarioRunner env) => RunMessage env TheEssexCountyExpress where
  runMessage msg s@(TheEssexCountyExpress attrs@ScenarioAttrs {..}) =
    case msg of
      SetTokensForScenario -> do
        standalone <- isNothing <$> getId @(Maybe CampaignId) ()
        s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
      Setup -> do
        investigatorIds <- getInvestigatorIds
        engineCar <-
          sample
          $ Locations.engineCar_175
          :| [Locations.engineCar_176, Locations.engineCar_177]
        trainCars <-
          zip
          <$> getRandoms
          <*> (take 6 <$> shuffleM
                [ Locations.passengerCar_167
                , Locations.passengerCar_168
                , Locations.passengerCar_169
                , Locations.passengerCar_170
                , Locations.passengerCar_171
                , Locations.sleepingCar
                , Locations.diningCar
                , Locations.parlorCar
                ]
              )
        encounterDeck <- buildEncounterDeck
          [ EncounterSet.TheEssexCountyExpress
          , EncounterSet.TheBeyond
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.DarkCult
          ]

        engineCarId <- getRandom

        let
          start = fromJustNote "No train cars?" $ headMay (map fst trainCars)
          end = fromJustNote "No train cars?" $ headMay $ reverse
            (map fst trainCars)
          allCars = map fst trainCars <> [engineCarId]
          token = case scenarioDifficulty of
            Easy -> MinusTwo
            Standard -> MinusThree
            Hard -> MinusFour
            Expert -> MinusFive

        pushAll
          $ [ story investigatorIds theEssexCountyExpressIntro
            , AddToken token
            , SetEncounterDeck encounterDeck
            , AddAgenda "02160"
            , AddAct "02165"
            ]
          <> concat
               [ [ PlaceLocation locationId cardDef
                 , SetLocationLabel locationId ("trainCar" <> tshow @Int n)
                 ]
               | (n, (locationId, cardDef)) <- zip [6, 5 ..] trainCars
               ]
          <> [ PlacedLocationDirection lid1 LeftOf lid2
             | (lid1, lid2) <- zip allCars (drop 1 allCars)
             ]
          <> [ PlaceLocation engineCarId engineCar
             , PlacedLocationDirection engineCarId RightOf end
             , CreateWindowModifierEffect
               EffectSetupWindow
               (EffectModifiers [Modifier (ScenarioSource scenarioId) Blank])
               (ScenarioSource scenarioId)
               (LocationTarget start)
             , RevealLocation Nothing start
             , MoveAllTo start
             ]

        let locations' = locationNameMap (engineCar : map snd trainCars)
        TheEssexCountyExpress
          <$> runMessage msg (attrs & locationsL .~ locations')
      ResolveToken _ Tablet iid | isEasyStandard attrs -> do
        closestCultists <- map unClosestEnemyId
          <$> getSetList (iid, [Trait.Cultist])
        s <$ case closestCultists of
          [] -> pure ()
          [x] -> push (PlaceDoom (EnemyTarget x) 1)
          xs -> push (chooseOne iid [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
      ResolveToken _ Tablet _ | isHardExpert attrs -> do
        cultists <- getSetList @EnemyId Trait.Cultist
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
        defeatedInvestigatorIds <- map unDefeatedInvestigatorId
          <$> getSetList ()
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
          <> [EndOfGame]
          )
      ScenarioResolution (Resolution 2) -> do
        msgs <- investigatorDefeat
        leadInvestigatorId <- getLeadInvestigatorId
        defeatedInvestigatorIds <- map unDefeatedInvestigatorId
          <$> getSetList ()
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
          <> [EndOfGame]
          )
      _ -> TheEssexCountyExpress <$> runMessage msg attrs
