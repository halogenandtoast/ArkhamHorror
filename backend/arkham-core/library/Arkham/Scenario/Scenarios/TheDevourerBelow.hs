module Arkham.Scenario.Scenarios.TheDevourerBelow where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.TheDevourerBelow.Story
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Id
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Attrs
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding (Cultist)

newtype TheDevourerBelow = TheDevourerBelow ScenarioAttrs
  deriving stock Generic
  deriving anyclass IsScenario
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasRecord env)

theDevourerBelow :: Difficulty -> TheDevourerBelow
theDevourerBelow difficulty =
  TheDevourerBelow $ (baseAttrs "01142" "The Devourer Below" difficulty)
    { scenarioLocationLayout = Just
      [ "woods1     .     woods2"
      , "woods1 mainPath woods2"
      , "woods3 mainPath woods4"
      , "woods3 ritualSite woods4"
      , "   .   ritualSite   .  "
      ]
    }

instance (HasTokenValue env InvestigatorId, Query EnemyMatcher env) => HasTokenValue env TheDevourerBelow where
  getTokenValue iid tokenFace (TheDevourerBelow attrs) = case tokenFace of
    Skull -> do
      monsterCount <- selectCount $ EnemyWithTrait Monster
      pure $ toTokenValue attrs Skull monsterCount 3
    Cultist -> pure $ toTokenValue attrs Cultist 2 4
    Tablet -> pure $ toTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toTokenValue attrs ElderThing 5 7
    otherFace -> getTokenValue iid otherFace attrs

actDeck :: [CardDef]
actDeck =
  [Acts.investigatingTheTrail, Acts.intoTheDarkness, Acts.disruptingTheRitual]

agendaDeck :: [CardDef]
agendaDeck =
  [Agendas.theArkhamWoods, Agendas.theRitualBegins, Agendas.vengeanceAwaits]

instance ScenarioRunner env => RunMessage env TheDevourerBelow where
  runMessage msg s@(TheDevourerBelow attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      pastMidnight <- getHasRecord ItIsPastMidnight
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
      ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest

      let
        woodsLabels = [ "woods" <> tshow @Int n | n <- [1 .. 4] ]
        ghoulPriestMessages =
          [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
        pastMidnightMessages =
          if pastMidnight then [AllRandomDiscard, AllRandomDiscard] else []
        cultistsWhoGotAwayMessages =
          replicate ((length cultistsWhoGotAway + 1) `div` 2) PlaceDoomOnAgenda

      mainPath <- genCard Locations.mainPath
      let mainPathId = toLocationId mainPath

      arkhamWoods <- traverse
        genCard
        [ Locations.arkhamWoodsUnhallowedGround
        , Locations.arkhamWoodsTwistingPaths
        , Locations.arkhamWoodsOldHouse
        , Locations.arkhamWoodsCliffside
        , Locations.arkhamWoodsTangledThicket
        , Locations.arkhamWoodsQuietGlade
        ]

      woodsLocations <- take 4 <$> shuffleM arkhamWoods

      randomSet <-
        sample
        $ EncounterSet.AgentsOfYogSothoth
        :| [ EncounterSet.AgentsOfShubNiggurath
           , EncounterSet.AgentsOfCthulhu
           , EncounterSet.AgentsOfHastur
           ]

      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.umordhoth]
        [ EncounterSet.TheDevourerBelow
        , EncounterSet.AncientEvils
        , EncounterSet.StrikingFear
        , EncounterSet.Ghouls
        , EncounterSet.DarkCult
        , randomSet
        ]

      pushAllEnd
        $ [ story investigatorIds intro
          , SetEncounterDeck encounterDeck
          , AddToken ElderThing
          , SetAgendaDeck
          , SetActDeck
          , PlaceLocation mainPath
          ]
        <> [ PlaceLocation card | card <- woodsLocations ]
        <> [ SetLocationLabel (LocationId $ toCardId card) label
           | (label, card) <- zip woodsLabels woodsLocations
           ]
        <> [ RevealLocation Nothing mainPathId
           , MoveAllTo (toSource attrs) mainPathId
           ]
        <> ghoulPriestMessages
        <> cultistsWhoGotAwayMessages
        <> pastMidnightMessages

      setAsideCards <- traverse
        genCard
        [Locations.ritualSite, Enemies.umordhoth]

      TheDevourerBelow <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL . at 1 ?~ actDeck)
        & (agendaStackL . at 1 ?~ agendaDeck)
        )
    ResolveToken _ Cultist iid -> do
      let doom = if isEasyStandard attrs then 1 else 2
      closestEnemyIds <- map unClosestEnemyId <$> getSetList iid
      case closestEnemyIds of
        [] -> pure ()
        [x] -> push (PlaceDoom (EnemyTarget x) doom)
        xs -> push (chooseOne iid [ PlaceDoom (EnemyTarget x) doom | x <- xs ])
      pure s
    ResolveToken _ Tablet iid -> do
      let horror = if isEasyStandard attrs then 0 else 1
      monsterCount <- selectCount $ EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid) <> EnemyWithTrait Monster
      s <$ when
        (monsterCount > 0)
        (push $ InvestigatorAssignDamage
          iid
          (TokenEffectSource Tablet)
          DamageAny
          1
          horror
        )
    ResolveToken _ ElderThing iid -> do
      ancientOneCount <- selectCount $ EnemyWithTrait AncientOne
      s <$ when (ancientOneCount > 0) (push $ DrawAnotherToken iid)
    FailedSkillTest iid _ _ (TokenTarget token) _ _
      | isHardExpert attrs && tokenFace token == Skull -> s <$ push
        (FindAndDrawEncounterCard
          iid
          (CardWithType EnemyType <> CardWithTrait Monster)
        )
    ScenarioResolution NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ push
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "Too frightened to face her fate, Lita flees\
                  \ into the night. She realizes that she has failed and Umôrdhoth’s\
                  \ vengeance will pursue her wherever she goes. The creature’s\
                  \ tendrils spread throughout the city of Arkham, searching for\
                  \ her. It lurks in the darkness of every corner, tugging at the seams\
                  \ of reality. But Lita is nowhere to be found, so the creature dwells\
                  \ in the shadows to this day, searching…killing"
                ]
              , Record ArkhamSuccumbedToUmordhothsTerribleVengeance
              ]
            <> [EndOfGame Nothing]
          ]
        )
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ push
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                (Just "Resolution 1")
                [ "You have managed to prevent the cult from\
                  \ summoning its master. Although you’re unsure what would\
                  \ have happened had the cult succeeded, you’re relieved that—at\
                  \ least for the time being—Arkham is safe. You capture as many\
                  \ cultists as you can find, but very few townspeople believe your\
                  \ tale. Perhaps it was all in your head, after all."
                ]
              , Record TheRitualToSummonUmordhothWasBroken
              ]
            <> [EndOfGame Nothing]
          ]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ push
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                (Just "Resolution 2")
                [ "Through force of arms and strength of will,\
                  \ you are somehow able to harm Umôrdhoth enough to send it\
                  \ reeling back to the dimension from which it emerged. Warmth\
                  \ and light return to the woods as the void-like mass is sucked in\
                  \ upon itself, vanishing in an instant. You aren’t sure if a being\
                  \ such as this can be killed, but for the time being it seems to have\
                  \ retreated. As their master vanishes, the ghouls nearby climb\
                  \ into the open pit below, fleeing with terrible cries and shrieks.\
                  \ You have stopped an evil plot, but the fight has taken its toll on\
                  \ your body and mind. Worse, you can’t help but feel insignificant\
                  \ in the face of the world’s mysteries. What other terrors exist in\
                  \ the deep, dark corners of reality?"
                ]
              , Record TheInvestigatorsRepelledUmordoth
              ]
            <> [EndOfGame Nothing]
          ]
        )
    ScenarioResolution (Resolution 3) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ push
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                (Just "Resolution 3")
                [ "In the face of this horror, you don’t believe there\
                  \ is anything you can do to stop it. You have but one hope if you\
                  \ are to survive. You turn on Lita and throw her at the terrible\
                  \ monstrosity, watching in dread as its swirling void-like mass\
                  \ consumes her. She cries out in torment as the life is sucked from\
                  \ her body. “Umôrdhoth…Umôrdhoth…” the cultists chant.\
                  \ Lita Chantler vanishes without a trace. For a moment, you\
                  \ fear that the creature will now turn on you, but you hear one of\
                  \ the cultists say, “Umôrdhoth is a just god who claims only the\
                  \ guilty and the dead. Go, and you shall be spared.” The swirling\
                  \ mass vanishes, and warmth and light return to the woods. The\
                  \ cultists slink away, leaving you alive. Lita’s last moments are\
                  \ forever etched upon your memory."
                ]
              , Record TheInvestigatorsSacrificedLitaChantlerToUmordhoth
              ]
            <> [EndOfGame Nothing]
          ]
        )
    _ -> TheDevourerBelow <$> runMessage msg attrs
