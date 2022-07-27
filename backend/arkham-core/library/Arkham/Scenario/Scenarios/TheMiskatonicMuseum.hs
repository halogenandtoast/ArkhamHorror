module Arkham.Scenario.Scenarios.TheMiskatonicMuseum where

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
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Name
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Target
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheMiskatonicMuseum = TheMiskatonicMuseum ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMiskatonicMuseum :: Difficulty -> TheMiskatonicMuseum
theMiskatonicMuseum difficulty = scenario
  TheMiskatonicMuseum
  "02118"
  "The Miskatonic Museum"
  difficulty
  [ ".     .     .                    .                    hall3 hall3          hall4          hall4 .                  .              .     ."
  , ".     .     hall2                hall2                hall3 hall3          hall4          hall4 hall5              hall5          .     ."
  , "hall1 hall1 hall2                hall2                .     museumHalls    museumHalls    .     hall5              hall5          hall6 hall6"
  , "hall1 hall1 .                    .                    .     museumHalls    museumHalls    .     .                  .              hall6 hall6"
  , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
  , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
  ]

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

instance HasTokenValue TheMiskatonicMuseum where
  getTokenValue iid tokenFace (TheMiskatonicMuseum attrs) = case tokenFace of
    Skull -> do
      huntingHorrorAtYourLocation <-
        selectAny $ enemyIs Enemies.huntingHorror <> EnemyAt
          (LocationWithInvestigator $ InvestigatorWithId iid)
      pure $ if huntingHorrorAtYourLocation
        then toTokenValue attrs Skull 3 4
        else toTokenValue attrs Skull 1 2
    Cultist -> pure $ toTokenValue attrs Cultist 1 3
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 5
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
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheMiskatonicMuseum where
  runMessage msg s@(TheMiskatonicMuseum attrs@ScenarioAttrs {..}) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
    LookAtTopOfDeck _ ScenarioDeckTarget n ->
      case fromJustNote "must be set" (lookup ExhibitDeck scenarioDecks) of
        xs -> do
          let lids = map (CardCodeTarget . toCardCode) $ take n xs
          s <$ pushAll [FocusTargets lids, Label "Continue" [UnfocusTargets]]
    Setup -> do
      investigatorIds <- getInvestigatorIds

      armitageKidnapped <- getHasRecordOrStandalone
        DrHenryArmitageWasKidnapped
        True

      exhibitHalls <- shuffleM =<< traverse
        genCard
        [ Locations.exhibitHallAthabaskanExhibit
        , Locations.exhibitHallMedusaExhibit
        , Locations.exhibitHallNatureExhibit
        , Locations.exhibitHallEgyptianExhibit
        , Locations.exhibitHallHallOfTheDead
        ]

      let (bottom, top) = splitAt 2 exhibitHalls

      restrictedHall <- genCard Locations.exhibitHallRestrictedHall

      bottom' <- shuffleM $ restrictedHall : bottom -- 02137 is the restricted hall

      let exhibitDeck = top <> bottom'

      encounterDeck <- buildEncounterDeckExcluding
        [Treacheries.shadowSpawned, Assets.haroldWalsted, Assets.adamLynch]
        [ EncounterSet.TheMiskatonicMuseum
        , EncounterSet.BadLuck
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.ChillingCold
        , EncounterSet.LockedDoors
        ]

      museumEntrance <- genCard Locations.museumEntrance
      museumHalls <- genCard Locations.museumHalls
      securityOffice <- genCard =<< sample
        (Locations.securityOffice_128 :| [Locations.securityOffice_129])
      administrationOffice <- genCard =<< sample
        (Locations.administrationOffice_130
        :| [Locations.administrationOffice_131]
        )

      pushAllEnd
        [ story investigatorIds theMiskatonicMuseumIntro1
        , story investigatorIds (theMiskatonicMuseumIntro2 armitageKidnapped)
        , SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , PlaceLocation securityOffice
        , PlaceLocation administrationOffice
        , PlaceLocation museumEntrance
        , PlaceLocation museumHalls
        , RevealLocation Nothing $ toLocationId museumEntrance
        , MoveAllTo (toSource attrs) $ toLocationId museumEntrance
        ]

      setAsideCards <- traverse
        genCard
        [ Assets.haroldWalsted
        , Assets.adamLynch
        , Assets.theNecronomiconOlausWormiusTranslation
        , Treacheries.shadowSpawned
        ]

      TheMiskatonicMuseum <$> runMessage
        msg
        (attrs
        & (decksL . at ExhibitDeck ?~ exhibitDeck)
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [ Acts.findingAWayInside
             , Acts.nightAtTheMuseum
             , Acts.breakingAndEntering
             , Acts.searchingForTheTome
             ]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.restrictedAccess
             , Agendas.shadowsDeepen
             , Agendas.inEveryShadow
             ]
          )
        )
    PlacedLocation name _ lid -> do
      s <$ if nameTitle name == "Exhibit Hall"
        then do
          hallCount <- selectCount $ LocationWithTitle "Exhibit Hall"
          push (SetLocationLabel lid $ "hall" <> tshow hallCount)
        else pure ()
    ResolveToken _ Tablet iid | isEasyStandard attrs ->
      s <$ push (InvestigatorPlaceCluesOnLocation iid 1)
    ResolveToken _ Tablet iid | isHardExpert attrs -> do
      lid <- getJustLocation iid
      mHuntingHorrorId <- getHuntingHorrorWith $ EnemyAt $ LocationWithId lid
      case mHuntingHorrorId of
        Just huntingHorrorId ->
          s <$ push (EnemyAttack iid huntingHorrorId DamageAny RegularAttack)
        Nothing -> pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ ->
      s <$ case tokenFace token of
        Cultist -> push
          $ FindEncounterCard iid (toTarget attrs) (CardWithCardCode "02141")
        ElderThing -> push $ ChooseAndDiscardAsset iid AnyAsset
        _ -> pure ()
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getJustLocation iid
      s <$ push (SpawnEnemyAt (EncounterCard ec) lid)
    FoundEnemyInVoid iid target eid | isTarget attrs target -> do
      lid <- getJustLocation iid
      s <$ push (EnemySpawnFromVoid Nothing lid eid)
    ScenarioResolution NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
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
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 1")
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
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 2")
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
                     [ AddCampaignCardToDeck
                         iid
                         Assets.theNecronomiconOlausWormiusTranslation
                     ]
                 | iid <- investigatorIds
                 ]
             ]
           , Label
             "Do not add The Necronomicon (Olaus Wormius Translation) to a deck"
             []
           ]
         , AddToken ElderThing
         ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    _ -> TheMiskatonicMuseum <$> runMessage msg attrs
