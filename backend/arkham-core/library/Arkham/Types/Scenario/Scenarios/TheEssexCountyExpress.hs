module Arkham.Types.Scenario.Scenarios.TheEssexCountyExpress
  ( TheEssexCountyExpress(..)
  , theEssexCountyExpress
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window
 hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token
import qualified Arkham.Types.Trait as Trait
import Data.List.NonEmpty (NonEmpty(..))

newtype TheEssexCountyExpress = TheEssexCountyExpress ScenarioAttrs
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
  , HasStep AgendaStep env
  )
  => HasTokenValue env TheEssexCountyExpress where
  getTokenValue (TheEssexCountyExpress attrs) iid = \case
    Skull -> do
      step <- unAgendaStep . getStep <$> ask
      pure $ toTokenValue attrs Skull step (step + 1)
    Cultist -> pure $ toTokenValue attrs Cultist 1 0
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 3
    otherFace -> getTokenValue attrs iid otherFace

standaloneTokens :: [Token]
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
  => ScenarioAttrs
  -> m [Message]
investigatorDefeat ScenarioAttrs {..} = do
  campaignStoryCards <- getList ()
  leadInvestigatorId <- getLeadInvestigatorId
  defeatedInvestigatorIds <- map unDefeatedInvestigatorId <$> getSetList ()
  let
    findOwner cardCode =
      (\iid -> iid <$ guard (iid `elem` defeatedInvestigatorIds))
        . campaignStoryCardInvestigatorId
        =<< find
              ((== cardCode) . getCardCode . campaignStoryCardPlayerCard)
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
                  Nothing
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
      <> [ AddCampaignCardToDeck iid "02178" | iid <- defeatedInvestigatorIds ]

instance ScenarioRunner env => RunMessage env TheEssexCountyExpress where
  runMessage msg s@(TheEssexCountyExpress attrs@ScenarioAttrs {..}) =
    case msg of
      SetTokensForScenario -> do
        standalone <- isNothing <$> getId @(Maybe CampaignId) ()
        s <$ if standalone
          then unshiftMessage (SetTokens standaloneTokens)
          else pure ()
      Setup -> do
        investigatorIds <- getInvestigatorIds
        engineCar <- sample $ "02175" :| ["02176", "02177"]
        trainCars <- take 6 <$> shuffleM
          [ "02167"
          , "02168"
          , "02169"
          , "02170"
          , "02171"
          , "02172"
          , "02173"
          , "02174"
          ]
        encounterDeck <- buildEncounterDeck
          [ EncounterSet.TheEssexCountyExpress
          , EncounterSet.TheBeyond
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.DarkCult
          ]

        let
          start = fromJustNote "No train cars?" $ headMay trainCars
          end = fromJustNote "No train cars?" $ headMay $ reverse trainCars
          allCars = trainCars <> [engineCar]
          token = case scenarioDifficulty of
            Easy -> MinusTwo
            Standard -> MinusThree
            Hard -> MinusFour
            Expert -> MinusFive

        unshiftMessages
          $ [ story investigatorIds theEssexCountyExpressIntro
            , AddToken token
            , SetEncounterDeck encounterDeck
            , AddAgenda "02160"
            , AddAct "02165"
            ]
          <> concat
               [ [ PlaceLocation location
                 , SetLocationLabel location ("trainCar" <> tshow @Int n)
                 ]
               | (n, location) <- zip [6, 5 ..] trainCars
               ]
          <> [ PlacedLocationDirection lid1 LeftOf lid2
             | (lid1, lid2) <- zip allCars (drop 1 allCars)
             ]
          <> [ PlaceLocation engineCar
             , PlacedLocationDirection engineCar RightOf end
             , CreateWindowModifierEffect
               EffectSetupWindow
               (EffectModifiers [Modifier (ScenarioSource scenarioId) Blank])
               (ScenarioSource scenarioId)
               (LocationTarget start)
             , RevealLocation Nothing start
             , MoveAllTo start
             ]

        let
          locations' = mapFromList $ map
            (second pure . toFst (getLocationName . lookupLocation))
            (engineCar : trainCars)
        TheEssexCountyExpress
          <$> runMessage msg (attrs & locationsL .~ locations')
      ResolveToken _ Tablet iid | isEasyStandard attrs -> do
        closestCultists <- map unClosestEnemyId
          <$> getSetList (iid, [Trait.Cultist])
        s <$ case closestCultists of
          [] -> pure ()
          [x] -> unshiftMessage (PlaceDoom (EnemyTarget x) 1)
          xs -> unshiftMessage
            (chooseOne iid [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
      ResolveToken _ Tablet _ | isHardExpert attrs -> do
        cultists <- getSetList @EnemyId Trait.Cultist
        s <$ unshiftMessages [ PlaceDoom (EnemyTarget eid) 1 | eid <- cultists ]
      FailedSkillTest iid _ _ (DrawnTokenTarget token) _ n ->
        s <$ case drawnTokenFace token of
          Cultist -> unshiftMessages
            [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
          ElderThing | isEasyStandard attrs ->
            unshiftMessage $ ChooseAndDiscardCard iid
          ElderThing | isHardExpert attrs ->
            unshiftMessages $ replicate n (ChooseAndDiscardCard iid)
          _ -> pure ()
      ScenarioResolution NoResolution ->
        s <$ unshiftMessages [ScenarioResolution $ Resolution 2]
      ScenarioResolution (Resolution 1) -> do
        msgs <- investigatorDefeat attrs
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        defeatedInvestigatorIds <- map unDefeatedInvestigatorId
          <$> getSetList ()
        xp <- getXp
        s <$ unshiftMessages
          (msgs
          <> [ chooseOne
                 leadInvestigatorId
                 [ Run
                     [ Continue "Continue"
                     , FlavorText
                       Nothing
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
                 (xp + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | iid <- investigatorIds
             ]
          <> [EndOfGame]
          )
      ScenarioResolution (Resolution 2) -> do
        msgs <- investigatorDefeat attrs
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        defeatedInvestigatorIds <- map unDefeatedInvestigatorId
          <$> getSetList ()
        xp <- getXp
        s <$ unshiftMessages
          (msgs
          <> [ chooseOne
               leadInvestigatorId
               [ Run
                   [ Continue "Continue"
                   , FlavorText
                     Nothing
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
                 (xp + (if iid `elem` defeatedInvestigatorIds then 1 else 0))
             | iid <- investigatorIds
             ]
          <> [EndOfGame]
          )
      _ -> TheEssexCountyExpress <$> runMessage msg attrs
