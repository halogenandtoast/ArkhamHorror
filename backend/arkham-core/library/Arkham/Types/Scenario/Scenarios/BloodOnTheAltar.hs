module Arkham.Types.Scenario.Scenarios.BloodOnTheAltar
  ( BloodOnTheAltar(..)
  , bloodOnTheAltar
  )
where

import Arkham.Import hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token
import Data.List.NonEmpty (NonEmpty(..))

newtype BloodOnTheAltar = BloodOnTheAltar Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bloodOnTheAltar :: Difficulty -> BloodOnTheAltar
bloodOnTheAltar difficulty = BloodOnTheAltar $ base
  { scenarioLocationLayout = Just
    [ ".                    houseInTheReeds      houseInTheReeds schoolhouse    schoolhouse         ."
    , "congregationalChurch congregationalChurch villageCommons  villageCommons osbornsGeneralStore osbornsGeneralStore"
    , ".                    burnedRuins          burnedRuins     bishopsBrook   bishopsBrook        ."
    ]
  }
 where
  base = baseAttrs
    "02195"
    "Blood on the Altar"
    ["02196", "02197", "02198"]
    ["02199", "02200"]
    difficulty

bloodOnTheAltarIntro :: Message
bloodOnTheAltarIntro = FlavorText
  (Just "Scenario IV: Blood on the Altar")
  [ "When you finally reach Dunwich, you are\
    \ greeted by Zebulon Whateley and Earl Sawyer,\
    \ another man from the village who had met with\
    \ Dr. Armitage during the incident several months\
    \ ago. “Things ain’t lookin’ too good here,” Earl\
    \ tells you. “Some folk up and went missin’ a few\
    \ nights ago. ‘Dem whippoorwills won’ shut up.\
    \ Dunno what yer doin’ here, but last time you Arkham folk came ‘round\
    \ it was bad news. Very bad news.” His eyes blink rapidly, and he coughs\
    \ and looks away."
  , "“Look, why don’t you rest fer the night an’ look fer whatever it is yer\
    \ looking fer t’morra,” Zebulon chimes in, putting a wrinkled hand on\
    \ your shoulder. You begin to protest, but your aching muscles and weary\
    \ mind won’t allow you to refuse. The elderly man offers to take you in\
    \ for the night, and drives you to his home at the outskirts of Dunwich\
    \ village. The town is disheveled and eerie, and you find yourself wishing\
    \ you hadn’t come here at all. You fall asleep on the ride over and scarcely\
    \ remember anything else from that night."
  , "When you awaken, you find that Zebulon’s house is abandoned, and\
    \ there is no sign of the elderly man, or of Mr. Sawyer. Fearing the worst,\
    \ you head into the village of Dunwich to investigate, hoping to find\
    \ answers."
  ]

instance
  ( HasTokenValue env InvestigatorId
  , HasSet LocationId env ()
  , HasSet UnderneathCardId env LocationId
  )
  => HasTokenValue env BloodOnTheAltar where
  getTokenValue (BloodOnTheAltar attrs) iid = \case
    Skull -> do
      numLocations <- countM ((null <$>) . getSetList @UnderneathCardId)
        =<< getSetList @LocationId ()
      pure $ toTokenValue attrs Skull (min 4 numLocations) numLocations
    Cultist -> pure $ toTokenValue attrs Cultist 2 4
    Tablet -> pure $ toTokenValue attrs Tablet 2 3
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

instance ScenarioRunner env => RunMessage env BloodOnTheAltar where
  runMessage msg s@(BloodOnTheAltar attrs@Attrs {..}) = case msg of
    SetTokensForScenario -> do
      standalone <- isNothing <$> getId @(Maybe CampaignId) ()
      s <$ if standalone
        then unshiftMessage (SetTokens standaloneTokens)
        else pure ()
    Setup -> do
      investigatorIds <- getInvestigatorIds
      bishopsBrook <- sample $ "02202" :| ["02203"]
      burnedRuins <- sample $ "02204" :| ["02205"]
      osbornsGeneralStore <- sample $ "02206" :| ["02207"]
      congregationalChurch <- sample $ "02208" :| ["02209"]
      houseInTheReeds <- sample $ "02210" :| ["02211"]
      schoolhouse <- sample $ "02212" :| ["02213"]
      encounterDeck <- buildEncounterDeck
        [ EncounterSet.BloodOnTheAltar
        , EncounterSet.Dunwich
        , EncounterSet.Whippoorwills
        , EncounterSet.Nightgaunts
        , EncounterSet.AncientEvils
        ]

      unshiftMessages
        [ story investigatorIds bloodOnTheAltarIntro
        , SetEncounterDeck encounterDeck
        , AddAgenda "02196"
        , AddAct "02199"
        , PlaceLocation "02201"
        , PlaceLocation bishopsBrook
        , PlaceLocation burnedRuins
        , PlaceLocation osbornsGeneralStore
        , PlaceLocation congregationalChurch
        , PlaceLocation houseInTheReeds
        , PlaceLocation schoolhouse
        , RevealLocation Nothing "02201"
        , MoveAllTo "02201"
        ]

      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocation))
          [ "02201"
          , bishopsBrook
          , burnedRuins
          , osbornsGeneralStore
          , congregationalChurch
          , houseInTheReeds
          , schoolhouse
          , "02214"
          ]
      BloodOnTheAltar <$> runMessage msg (attrs & locationsL .~ locations')
    ResolveToken _ Tablet iid -> do
      lid <- getId @LocationId iid
      matches <- (== "Hidden Chamber") . nameTitle <$> getName lid
      s <$ when
        (isHardExpert attrs || (isEasyStandard attrs && matches))
        (unshiftMessage $ DrawAnotherToken iid)
    ResolveToken _ ElderThing _ | isHardExpert attrs -> do
      agendaId <- fromJustNote "no agenda" . headMay <$> getSetList @AgendaId ()
      s <$ unshiftMessage (PlaceDoom (AgendaTarget agendaId) 1)
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _ ->
      s <$ case drawnTokenFace token of
        Cultist -> do
          lid <- getId @LocationId iid
          unshiftMessage (PlaceClues (LocationTarget lid) 1)
        ElderThing | isEasyStandard attrs -> do
          agendaId <-
            fromJustNote "no agenda" . headMay <$> getSetList @AgendaId ()
          unshiftMessage (PlaceDoom (AgendaTarget agendaId) 1)
        _ -> pure ()
    NoResolution -> s <$ unshiftMessages [Resolution 2]
    Resolution 1 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      defeatedInvestigatorIds <- map unDefeatedInvestigatorId <$> getSetList ()
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
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
    Resolution 2 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      defeatedInvestigatorIds <- map unDefeatedInvestigatorId <$> getSetList ()
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
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
    _ -> BloodOnTheAltar <$> runMessage msg attrs
