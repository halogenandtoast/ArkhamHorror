module Arkham.Types.Scenario.Scenarios.TheHouseAlwaysWins
  ( TheHouseAlwaysWins(..)
  , theHouseAlwaysWins
  )
where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.ScenarioLogKey

newtype TheHouseAlwaysWins = TheHouseAlwaysWins ScenarioAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theHouseAlwaysWins :: Difficulty -> TheHouseAlwaysWins
theHouseAlwaysWins difficulty =
  TheHouseAlwaysWins $ (baseAttrs
                         "02062"
                         "The House Always Wins"
                         ["02063", "02064", "02065"]
                         ["02066", "02067", "02068", "02069"]
                         difficulty
                       )
    { scenarioLocationLayout = Just
      [ ".           .                .             .                  backHallDoorway1 ."
      , "laBellaLuna cloverClubLounge cloverClubBar cloverClubCardroom darkenedHall     backHallDoorway2"
      , ".           .                .             .                  backHallDoorway3 ."
      ]
    }

theHouseAlwaysWinsIntro :: Message
theHouseAlwaysWinsIntro = FlavorText
  (Just "Scenario I-B: The House Always Wins")
  [ "Dr. Armitage suggested you track down his associate Dr. Francis Morgan.\
    \ He’s not sure whether Dr. Morgan is in trouble, but he’s not particularly\
    \ happy with his colleague’s present choice of company. He’s in the Clover Club,\
    \ a notorious gambling joint somewhere downtown. Finding the club’s exact\
    \ location isn’t easy—you have to grease a few palms just to learn which of the\
    \ Downtown restaurants operates as the club’s front. That restaurant is La\
    \ Bella Luna, a somewhat upscale Italian eatery by the theatre. You change into\
    \ your Sunday best and make your way there."
  , "In front of La Bella Luna stands a man in a pinstripe suit who sizes you up as\
    \ you approach. “Enjoy yourselves,” he says with a snake-like grin as he holds\
    \ open the restaurant’s front door."
  ]

instance HasTokenValue env InvestigatorId => HasTokenValue env TheHouseAlwaysWins where
  getTokenValue (TheHouseAlwaysWins attrs) iid = \case
    Skull -> pure $ toTokenValue attrs Skull 2 3
    Cultist -> pure $ TokenValue Cultist (NegativeModifier 3)
    Tablet -> pure $ TokenValue Tablet (NegativeModifier 2)
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env TheHouseAlwaysWins where
  runMessage msg s@(TheHouseAlwaysWins attrs@ScenarioAttrs {..}) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeck
        [ EncounterSet.TheHouseAlwaysWins
        , EncounterSet.BadLuck
        , EncounterSet.NaomisCrew
        , EncounterSet.Rats
        ]
      cloverClubPitBoss <- buildCard "02078"
      pushMessages
        [ SetEncounterDeck encounterDeck
        , AddAgenda "02063"
        , AddAct "02066"
        , PlaceLocation "02070"
        , PlaceLocation "02071"
        , PlaceLocation "02072"
        , PlaceLocation "02073"
        , RevealLocation Nothing "02070"
        , MoveAllTo "02070"
        , CreateEnemyAt cloverClubPitBoss "02071"
        , AskMap
        . mapFromList
        $ [ ( iid
            , ChooseOne [Run [Continue "Continue", theHouseAlwaysWinsIntro]]
            )
          | iid <- investigatorIds
          ]
        ]
      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocation))
          [ "02070"
          , "02071"
          , "02072"
          , "02073"
          , "02074"
          , "02075"
          , "02076"
          , "02077"
          ]
      TheHouseAlwaysWins <$> runMessage msg (attrs & locationsL .~ locations')
    ResolveToken _ Tablet iid -> s <$ unshiftMessage (SpendResources iid 3)
    ResolveToken drawnToken Skull iid -> do
      let requiredResources = if isEasyStandard attrs then 2 else 3
      resourceCount <- unResourceCount <$> getCount iid
      if resourceCount >= requiredResources
        then unshiftMessage $ chooseOne
          iid
          [ Label
            ("Spend "
            <> tshow requiredResources
            <> " resources to treat this token as a 0"
            )
            [ SpendResources iid requiredResources
            , CreateTokenValueEffect
              (if isEasyStandard attrs then 2 else 3)
              (DrawnTokenSource drawnToken)
              (DrawnTokenTarget drawnToken)
            ]
          , Label "Do not spend resources" []
          ]
        else pure ()
      s <$ unshiftMessage (SpendResources iid 3)
    PassedSkillTest iid _ _ (DrawnTokenTarget token) _ _ ->
      s <$ case drawnTokenFace token of
        Cultist | isEasyStandard attrs ->
          unshiftMessage $ TakeResources iid 3 False
        _ -> pure ()
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _ ->
      s <$ case drawnTokenFace token of
        Cultist | isHardExpert attrs -> unshiftMessage $ SpendResources iid 3
        Tablet | isEasyStandard attrs -> unshiftMessage $ SpendResources iid 3
        _ -> pure ()
    ScenarioResolution NoResolution ->
      s <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      cheated <-
        any
            (\case
              Cheated{} -> True
              _ -> False
            )
          <$> getSet ()
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "You flee to the end of the block and pause to\
                  \ recover. Before you can catch your breath, the ground shakes\
                  \ with a thunderous crash. People emerge from their homes and\
                  \ storefronts to see what the ruckus is, and a crowd forms on\
                  \ the street. You head to the front of the crowd and are horrified\
                  \ to see the building from which you fled just minutes earlier\
                  \ reduced to rubble. There is no sign of Dr. Morgan anywhere."
                 ]
               ]
           ]
         , Record OBannionGangHasABoneToPickWithTheInvestigators
         , Record DrFrancisMorganWasKidnapped
         ]
        <> [ AddToken Tablet | cheated ]
        <> [ GainXP iid (xp + 1) | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      cheated <-
        any
            (\case
              Cheated{} -> True
              _ -> False
            )
          <$> getSet ()
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "“What in the world…?” Dr. Morgan finally\
                  \ breaks out of his daze as you make your way to safety. You ask\
                  \ him what he remembers, and he sputters and shakes his head.\
                  \   “It’s all a haze,” he explains, visibly exhausted. “I was having\
                  \ the run of my life! Perhaps I had one too many shots. But,\
                  \ those creatures—I haven’t seen anything like that since…” He\
                  \ trails off, and you can tell that his mind is racing. His eyes widen\
                  \ with realization and his face pales. “I may not be in the best\
                  \ shape, but I’ll help with your investigation. Whatever it takes.”"
                 ]
               ]
           ]
         , Record OBannionGangHasABoneToPickWithTheInvestigators
         , Record TheInvestigatorsRescuedDrFrancisMorgan
         , chooseOne
           leadInvestigatorId
           [ Label
             "Add Dr. Francis Morgan to a deck"
             [ chooseOne
                 leadInvestigatorId
                 [ TargetLabel
                     (InvestigatorTarget iid)
                     [AddCampaignCardToDeck iid "02061"]
                 | iid <- investigatorIds
                 ]
             ]
           , Label "Do not add Dr. Francis Morgan to any deck" []
           ]
         ]
        <> [ AddToken Tablet | cheated ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 3) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      cheated <-
        any
            (\case
              Cheated{} -> True
              _ -> False
            )
          <$> getSet ()
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "Although you were unable to find Dr.\
                  \ Morgan in the club, the man you rescued is grateful for your\
                  \ help. He introduces himself as Peter Clover, the owner of\
                  \ the establishment you’d just left. Despite the situation, he\
                  \ maintains an air of quiet professionalism. As you make your\
                  \ way towards the street, a well-polished Chrysler B-70 rolls\
                  \ up to you, and a gorgeous woman with long brown hair and\
                  \ narrow eyes exits. She is flanked by dangerous-looking men\
                  \ who slip their hands under their suit jackets when they see\
                  \ you. “Peter,” she says with a sigh of relief, “Good, you’re okay.\
                  \   I heard there was trouble?” She turns and glares at you with\
                  \ deadly eyes. “Who are they?”"
                 , "Mr. Clover dusts off his vest, unworried. “Naomi, my dear, these\
                  \ are friends of mine. They…” he clears his throat. “They escorted\
                  \ me off the premises,” he explains after a short pause. “They have\
                  \ earned our gratitude.” The woman crosses her arms and takes a\
                  \ moment to size you up before giving you a smirk."
                 , "“Very well then. I must thank you for taking care of Peter. Run\
                  \ along now; we’ll handle things from here.” She nods to the\
                  \ goons flanking her and they walk past you toward the club’s\
                  \ rear entrance, pulling firearms out from underneath their coats.\
                  \   You’re not sure what ‘handling things’ means, but you’re pretty\
                  \ sure you don’t want to be here when the gunfire starts. You\
                  \ thank Naomi and Peter, and head off."
                 ]
               ]
           ]
         , Record NaomiHasTheInvestigatorsBacks
         , Record DrFrancisMorganWasKidnapped
         ]
        <> [ AddToken Tablet | cheated ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 4) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      cheated <-
        any
            (\case
              Cheated{} -> True
              _ -> False
            )
          <$> getSet ()
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "You are pulled from the debris by several\
                  \ firefighters, one of whom exclaims, “We’ve got a live one!”\
                  \ A few of them patch you up, and the cops ask you what\
                  \ happened. You’re certain they wouldn’t believe your story\
                  \ about horrible monstrosities demolishing the building from\
                  \ within. Unsure of what to say, you give a vague statement\
                  \ about not remembering much. “We’re bringing you to\
                  \ St. Mary’s,” one of the nurses says, pointing to a nearby\
                  \ ambulance. Knowing now how dire the situation is, you slip\
                  \ away while she is distracted by something else in the rubble…"
                   ]
                 , Record OBannionGangHasABoneToPickWithTheInvestigators
                 , Record DrFrancisMorganWasKidnapped
                 , Record InvestigatorsWereUnconsciousForSeveralHours
                 ]
             ]
         ]
        <> [ AddToken Tablet | cheated ]
        <> [ GainXP iid (xp + 1) | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    _ -> TheHouseAlwaysWins <$> runMessage msg attrs
