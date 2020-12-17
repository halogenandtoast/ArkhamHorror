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
import Data.UUID.V4
import System.Random.Shuffle

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

      armitageKidnapped <- getHasRecord DrHenryArmitageWasKidnapped

      exhibitHalls <- liftIO $ shuffleM =<< for
        ["02132", "02133", "02134", "02135", "02136"]
        (\cardCode -> lookupEncounterCard cardCode . CardId <$> nextRandom)

      restrictedHall <-
        liftIO $ lookupEncounterCard "02137" . CardId <$> nextRandom

      let (bottom, top) = splitAt 2 exhibitHalls

      bottom' <- liftIO $ shuffleM $ restrictedHall : bottom

      let exhibitDeck = top <> bottom'

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
      TheMiskatonicMuseum <$> runMessage
        msg
        (attrs & locationsL .~ locations' & deckL ?~ exhibitDeck)
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
                 [ "Whatever the creature in the\
                   \ museum was, you had neither the will nor the tools to destroy\
                   \ it. It seems you must give up any hope of recovering the\
                   \ Necronomicon. Even so, there are others depending on you.\
                   \ Gathering your courage, you prepare for your next task."
                 ]
               ]
           ]
         , Record TheInvestigatorsFailedToRecoverTheNecronomicon
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
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
                 [ "As long as this translation of the\
                   \ Necronomicon exists, there will be sorcerers and other foul\
                   \ agents like Whateley seeking it. In the end, you know what\
                   \ must be done to protect humanity from the threats you’ve seen.\
                   \ You find a trash bin and fill it with books and documents,\
                   \ throwing the Necronomicon on top. It takes several matches\
                   \ to set the contents of the bin alight. The flames fill the room\
                   \ with heat and the creeping shadows retreat from its light. You\
                   \ watch the book burn for some time, its pages turning to ash.\
                   \ You can only hope you’ve made the right decision."
                 ]
               ]
           ]
         , Record TheInvestigatorsDestroyedTheNecronomicon
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
                 [ "The Necronomicon is more than just a book;\
                   \ it is a tool. Within its pages is a wealth of information about\
                   \ the forces and creatures you have encountered. Knowing how\
                   \ useful it could be in your endeavors, how could you possibly\
                   \ bring yourself to destroy it? Besides, as long as you keep the\
                   \ book safely in your possession, you will still be foiling those\
                   \ who wish to use it for nefarious purposes."
                 ]
               ]
           ]
         , Record TheInvestigatorsTookCustodyOfTheNecronomicon
         , chooseOne
           leadInvestigatorId
           [ Label
             "Add The Necronomicon (Olaus Wormius Translation) to a deck"
             [ chooseOne
                 leadInvestigatorId
                 [ TargetLabel
                     (InvestigatorTarget iid)
                     [AddCampaignCardToDeck iid "02140"]
                 | iid <- investigatorIds
                 ]
             ]
           , Label
             "Do not add The Necronomicon (Olaus Wormius Translation) to a deck"
             []
           ]
         , AddToken ElderThing
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    _ -> TheMiskatonicMuseum <$> runMessage msg attrs
