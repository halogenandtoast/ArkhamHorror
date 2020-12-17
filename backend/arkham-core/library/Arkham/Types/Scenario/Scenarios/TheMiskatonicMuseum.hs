{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Scenario.Scenarios.TheMiskatonicMuseum where

import Arkham.Import hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token
import Data.List.NonEmpty (NonEmpty(..))

newtype TheMiskatonicMuseum = TheMiskatonicMuseum Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMiskatonicMuseum :: Difficulty -> TheMiskatonicMuseum
theMiskatonicMuseum difficulty = TheMiskatonicMuseum $ baseAttrs
  "02118"
  "The Miskatonic Museum"
  ["02119", "02120", "02121"]
  ["02122", "02123", "02124", "02125"]
  difficulty

theMiskatonicMuseumIntro1 :: Message
theMiskatonicMuseumIntro1 = FlavorText
  (Just "Scenario II: The Miskatonic Museum")
  [ "Several months ago, Armitage and his colleagues\
    \ stopped a rampaging horror from tearing\
    \ through Dunwich, a backwater town several\
    \ hours north and west of Arkham. At first you\
    \ imagine this beast as a rabid bear, or worse, but\
    \ the professor’s description of the creature paints\
    \ a different picture."
  , "It all began when a man named Wilbur Whateley entered the Orne\
    \ Library looking for Olaus Wormius’s Latin translation of a book called\
    \ the Necronomicon. Wilbur already possessed a beaten-up English\
    \ translation by Dr. John Dee, but it was insufficient for his purposes.\
    \   Armitage turned the man away, fearing what use the strange man had\
    \ for the book. Whateley returned in secret, hoping to steal the book ,\
    \ but was attacked by a hound guarding the university. Armitage, Rice,\
    \ and Morgan later discovered Whateley’s body. A description of the foul\
    \ corpse—semi-anthropomorphic and covered in fur, with a leathery\
    \ hide and greenish-grey tentacles—causes you to question whether or\
    \ not Whateley was truly human."
  ]

theMiskatonicMuseumIntro2 :: Bool -> Message
theMiskatonicMuseumIntro2 True = FlavorText
  Nothing
  [ "The notes written by Dr. Armitage in the journal stress\
    \ Whateley’s desire to get his hands on the Necronomicon for some\
    \ terrible purpose. As you read on, it seems that Dr. Armitage brought\
    \ the university’s copy of the tome to Harold Walsted—the curator of\
    \ the Miskatonic Museum—for safekeeping in the museum’s Restricted\
    \ Hall. Although you are worried about your mentor, you are equally\
    \ worried that Armitage’s kidnappers might get their hands on this\
    \ Necronomicon. You decide to head to the museum to prevent them\
    \ from acquiring it."
  ]
theMiskatonicMuseumIntro2 False = FlavorText
  Nothing
  [ "“My colleagues and I were quick to put the ordeal behind\
    \ us,” Armitage says with a sigh. “But it seems that things haven’t\
    \ fully resolved themselves. I’ll tell you the rest later, but for now, it is\
    \ imperative that we get our hands on that copy of the Necronomicon.\
    \ If my instincts are correct, the assailants you’ve encountered will be\
    \ searching for it. After all that transpired, I didn’t feel safe keeping it\
    \ at the library, so I brought it to my good friend, Harold Walsted. He is\
    \ the current curator of the Miskatonic Museum. I thought it would be\
    \ safe in the museum’s Restricted Hall, but now I’m not so sure. You must\
    \ retrieve it at all costs! I fear terribly what they could do with the rites\
    \                                                        contained in its pages…”"
  ]

instance
  ( HasTokenValue env InvestigatorId
  , HasId CardCode env EnemyId
  , HasId LocationId env InvestigatorId
  , HasSet EnemyId env LocationId
  )
  => HasTokenValue env TheMiskatonicMuseum where
  getTokenValue (TheMiskatonicMuseum attrs) iid = \case
    Skull -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList @EnemyId lid
      huntingHorrorAtYourLocation <- elem "02141"
        <$> for enemyIds (getId @CardCode)
      pure . TokenValue Skull . NegativeModifier $ if isEasyStandard attrs
        then (if huntingHorrorAtYourLocation then 3 else 1)
        else (if huntingHorrorAtYourLocation then 4 else 2)
    Cultist -> pure $ TokenValue
      Cultist
      (NegativeModifier $ if isEasyStandard attrs then 1 else 3)
    Tablet -> pure $ TokenValue
      Tablet
      (NegativeModifier $ if isEasyStandard attrs then 2 else 4)
    ElderThing -> pure $ TokenValue
      Tablet
      (NegativeModifier $ if isEasyStandard attrs then 3 else 5)
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env TheMiskatonicMuseum where
  runMessage msg s@(TheMiskatonicMuseum attrs@Attrs {..}) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds

      securityOffice <- liftIO $ sample $ "02128" :| ["02129"]
      administrationOffice <- liftIO $ sample $ "02130" :| ["02131"]

      armitageKidnapped <- asks $ hasRecord DrHenryArmitageWasKidnapped

      encounterDeck <- buildEncounterDeck
        [ EncounterSet.TheMiskatonicMuseum
        , EncounterSet.BadLuck
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.ChillingCold
        , EncounterSet.LockedDoors
        ]
      pushMessages
        [ story investigatorIds theMiskatonicMuseumIntro1
        , story investigatorIds (theMiskatonicMuseumIntro2 armitageKidnapped)
        , SetEncounterDeck encounterDeck
        , AddAgenda "02119"
        , AddAct "02122"
        , PlaceLocation securityOffice
        , PlaceLocation administrationOffice
        , PlaceLocation "02126"
        , PlaceLocation "02127"
        , RevealLocation Nothing "02126"
        , MoveAllTo "02126"
        ]
      let
        locations' = mapFromList
          [ ("Museum Entrance", ["02126"])
          , ("Museum Halls", ["02127"])
          , ("Security Office", [securityOffice])
          , ("Administration Office", [administrationOffice])
          , ( "Exhibit Hall"
            , ["02132", "02133", "02134", "02135", "02136", "02137"]
            )
          ]
      TheMiskatonicMuseum <$> runMessage msg (attrs & locations .~ locations')
    ResolveToken _ ElderThing iid | isEasyStandard attrs ->
      s <$ unshiftMessage (InvestigatorPlaceCluesOnLocation iid 1)
    ResolveToken _ ElderThing iid | isHardExpert attrs -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList @EnemyId lid
      mHuntingHorrorId <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorrorId of
        Just huntingHorrorId -> s <$ when
          (huntingHorrorId `elem` enemyIds)
          (unshiftMessage $ EnemyAttack iid huntingHorrorId)
        Nothing -> pure s
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ ->
      s <$ case drawnTokenFace token of
        Cultist -> unshiftMessage $ FindEncounterCard
          iid
          (toTarget attrs)
          (EncounterCardMatchByCardCode "02141")
        ElderThing -> unshiftMessage $ ChooseAndDiscardAsset iid
        _ -> pure ()
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getId @LocationId iid
      s <$ unshiftMessage (SpawnEnemyAt (EncounterCard ec) lid)
    NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "As you flee from the university,\
                  \ you hear screaming from the northern end of the campus. An\
                  \ ambulance passes you by, and you fear the worst. Hours later,\
                  \ you learn that a ‘rabid dog of some sort’ found its way into\
                  \ the university dormitories. The creature attacked the students\
                  \ inside and many were mauled or killed in the attack."
                 ]
               ]
           ]
         , Record ProfessorWarrenRiceWasKidnapped
         , Record TheInvestigatorsFailedToSaveTheStudents
         , AddToken Tablet
         ]
        <> [ GainXP iid (xp + 1) | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    Resolution 1 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "You find Professor Rice bound and gagged\
                  \ in the closet of his office. When you free him, he informs you\
                  \ that the strange men and women wandering around the\
                  \ campus had been stalking him for hours. They cornered him\
                  \ in his office and tied him up, although for what purpose, Rice\
                  \ isn’t sure. You inform him that Dr. Armitage sent you, and\
                  \ Rice looks relieved, although he suspects that Dr. Morgan\
                  \ might be in danger as well. Because the strangers on campus\
                  \ seem to have been targeting Professor Rice, you decide that\
                  \ the best course of action is to escort him away from the\
                  \ campus as quickly as possible. As you leave the university,\
                  \ you hear screaming from the northern end of the campus. An\
                  \ ambulance passes you by, and you fear the worst. Hours later,\
                  \ you learn that a ‘rabid dog of some sort’ found its way into\
                  \ the university dormitories. The creature attacked the students\
                  \ inside, and many were mauled or killed in the attack."
                 ]
               ]
           ]
         , Record TheInvestigatorsRescuedProfessorWarrenRice
         , AddToken Tablet
         , chooseOne
           leadInvestigatorId
           [ Label
             "Add Professor Warren Rice to your deck"
             [AddCampaignCardToDeck leadInvestigatorId "02061"]
           , Label "Do not add Professor Warren Rice to your deck" []
           ]
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    Resolution 2 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "You pull each of the dormitory’s fire alarms\
                  \ and usher the students out of the building’s north exit,\
                  \ hoping to make your way off campus. Many of the students\
                  \ are confused and exhausted, but you believe an attempt to\
                  \ explain the situation will do more harm than good. Minutes\
                  \ later, a terrible screech echoes across the campus, piercing\
                  \ and shrill. You tell the students to wait and head back to the\
                  \ dormitories to investigate. Oddly, you find no trace of the\
                  \ strange creature—a prospect that worries you more than it\
                  \ relieves you. You hurry to the faculty offices to find Professor\
                  \ Rice, but there is no sign of him anywhere."
                 ]
               ]
           ]
         , Record ProfessorWarrenRiceWasKidnapped
         , Record TheStudentsWereRescued
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    Resolution 3 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "After defeating the strange and terrifying\
                  \ creature from the Department of Alchemy, you rush to the\
                  \ faculty offices to find Professor Rice. By the time you get to his\
                  \ office, there is no sign of him anywhere."
                 ]
               ]
           ]
         , Record ProfessorWarrenRiceWasKidnapped
         , Record TheExperimentWasDefeated
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    Resolution 4 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "You awaken hours later, exhausted and\
                  \ injured. You’re not sure what you saw, but the sight of it filled\
                  \ your mind with terror. From other survivors, you learn that\
                  \ a ‘rabid dog of some sort’ found its way into the university\
                  \ dormitories. The creature attacked the students inside, and\
                  \ many were mauled or killed in the attack."
                 ]
               ]
           ]
         , Record InvestigatorsWereUnconsciousForSeveralHours
         , Record ProfessorWarrenRiceWasKidnapped
         , Record TheInvestigatorsFailedToSaveTheStudents
         , AddToken Tablet
         ]
        <> [ GainXP iid (xp + 1) | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    _ -> TheMiskatonicMuseum <$> runMessage msg attrs
