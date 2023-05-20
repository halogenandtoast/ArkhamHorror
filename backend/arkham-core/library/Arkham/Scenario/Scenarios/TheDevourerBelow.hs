module Arkham.Scenario.Scenarios.TheDevourerBelow where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheDevourerBelow.Story
import Arkham.Token
import Arkham.Trait hiding (Cultist)

newtype TheDevourerBelow = TheDevourerBelow ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDevourerBelow :: Difficulty -> TheDevourerBelow
theDevourerBelow difficulty =
  scenario
    TheDevourerBelow
    "01142"
    "The Devourer Below"
    difficulty
    [ "woods1     .     woods2"
    , "woods1 mainPath woods2"
    , "woods3 mainPath woods4"
    , "woods3 ritualSite woods4"
    , "   .   ritualSite   .  "
    ]

instance HasTokenValue TheDevourerBelow where
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

instance RunMessage TheDevourerBelow where
  runMessage msg s@(TheDevourerBelow attrs) = case msg of
    Setup -> do
      investigatorIds <- allInvestigatorIds
      pastMidnight <- getHasRecord ItIsPastMidnight
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
      ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest

      let
        woodsLabels = ["woods" <> tshow @Int n | n <- [1 .. 4]]
        ghoulPriestMessages =
          [AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive]
        pastMidnightMessages =
          if pastMidnight
            then
              [ AllRandomDiscard (toSource attrs) AnyCard
              , AllRandomDiscard (toSource attrs) AnyCard
              ]
            else []
        cultistsWhoGotAwayMessages =
          replicate
            ((length cultistsWhoGotAway + 1) `div` 2)
            PlaceDoomOnAgenda

      (mainPathId, placeMainPath) <- placeLocationCard Locations.mainPath

      arkhamWoods <-
        genCards
          [ Locations.arkhamWoodsUnhallowedGround
          , Locations.arkhamWoodsTwistingPaths
          , Locations.arkhamWoodsOldHouse
          , Locations.arkhamWoodsCliffside
          , Locations.arkhamWoodsTangledThicket
          , Locations.arkhamWoodsQuietGlade
          ]

      woodsLocations <- take 4 <$> shuffleM arkhamWoods

      randomSet <-
        sample $
          EncounterSet.AgentsOfYogSothoth
            :| [ EncounterSet.AgentsOfShubNiggurath
               , EncounterSet.AgentsOfCthulhu
               , EncounterSet.AgentsOfHastur
               ]

      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.umordhoth]
          [ EncounterSet.TheDevourerBelow
          , EncounterSet.AncientEvils
          , EncounterSet.StrikingFear
          , EncounterSet.Ghouls
          , EncounterSet.DarkCult
          , randomSet
          ]

      placeWoods <-
        for (zip woodsLabels woodsLocations) $ \(label, location) -> do
          (locationId, placement) <- placeLocation location
          pure [placement, SetLocationLabel locationId label]

      pushAll $
        [ story investigatorIds intro
        , SetEncounterDeck encounterDeck
        , AddToken ElderThing
        , SetAgendaDeck
        , SetActDeck
        , placeMainPath
        ]
          <> concat placeWoods
          <> [ RevealLocation Nothing mainPathId
             , MoveAllTo (toSource attrs) mainPathId
             ]
          <> ghoulPriestMessages
          <> cultistsWhoGotAwayMessages
          <> pastMidnightMessages

      setAsideCards <- genCards [Locations.ritualSite, Enemies.umordhoth]

      acts <- genCards actDeck
      agendas <- genCards agendaDeck

      TheDevourerBelow
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveToken _ Cultist iid -> do
      let doom = if isEasyStandard attrs then 1 else 2
      closestEnemyIds <- selectList $ NearestEnemy AnyEnemy
      case closestEnemyIds of
        [] -> pure ()
        [x] -> push $ PlaceDoom (TokenEffectSource Cultist) (toTarget x) doom
        xs ->
          push $
            chooseOne
              iid
              [targetLabel x [PlaceDoom (TokenEffectSource Cultist) (toTarget x) doom] | x <- xs]
      pure s
    ResolveToken _ Tablet iid -> do
      let horror = if isEasyStandard attrs then 0 else 1
      isMonsterAtYourLocation <-
        selectAny $
          EnemyAt (locationWithInvestigator iid)
            <> EnemyWithTrait Monster
      when isMonsterAtYourLocation $ do
        push $
          InvestigatorAssignDamage
            iid
            (TokenEffectSource Tablet)
            DamageAny
            1
            horror
      pure s
    ResolveToken _ ElderThing iid -> do
      anyAncientOnes <- selectAny $ EnemyWithTrait AncientOne
      s <$ when anyAncientOnes (push $ DrawAnotherToken iid)
    FailedSkillTest iid _ _ (TokenTarget (tokenFace -> Skull)) _ _
      | isHardExpert attrs -> do
          push $
            FindAndDrawEncounterCard
              iid
              (CardWithType EnemyType <> CardWithTrait Monster)
              True
          pure s
    ScenarioResolution r -> do
      let
        (resolution, record) = case r of
          NoResolution ->
            (noResolution, ArkhamSuccumbedToUmordhothsTerribleVengeance)
          Resolution 1 -> (resolution1, TheRitualToSummonUmordhothWasBroken)
          Resolution 2 -> (resolution2, TheInvestigatorsRepelledUmordoth)
          Resolution 3 ->
            (resolution3, TheInvestigatorsSacrificedLitaChantlerToUmordhoth)
          _ -> error "Invalid resolution"
      iids <- allInvestigatorIds
      pushAll [story iids resolution, Record record, EndOfGame Nothing]
      pure s
    _ -> TheDevourerBelow <$> runMessage msg attrs
