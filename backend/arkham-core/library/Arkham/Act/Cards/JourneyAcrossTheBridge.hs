module Arkham.Act.Cards.JourneyAcrossTheBridge (
  JourneyAcrossTheBridge (..),
  journeyAcrossTheBridge,
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message (createEnemy)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message (toMessage)
import Arkham.Placement
import Arkham.Scenario.Types (Field (..))
import Data.List.NonEmpty qualified as NE

newtype JourneyAcrossTheBridge = JourneyAcrossTheBridge ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

journeyAcrossTheBridge :: ActCard JourneyAcrossTheBridge
journeyAcrossTheBridge = act (1, A) JourneyAcrossTheBridge Cards.journeyAcrossTheBridge Nothing

instance HasAbilities JourneyAcrossTheBridge where
  getAbilities (JourneyAcrossTheBridge attrs) =
    [ restrictedAbility
      attrs
      1
      (EachUndefeatedInvestigator $ at_ (LocationWithLabel "theGreatWeb4"))
      (Objective $ FastAbility $ GroupClueCost (PerPlayer 3) (LocationWithLabel "theGreatWeb4"))
    | onSide A attrs
    ]

instance RunMessage JourneyAcrossTheBridge where
  runMessage msg a@(JourneyAcrossTheBridge attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      story $ i18n "theDreamEaters.weaverOfTheCosmos.theSpiderQueen1"
      survived <- getHasRecord RandolphSurvivedTheDescent
      knowsTheTruth <- getHasRecord TheBlackCatKnowsTheTruth

      when (survived && knowsTheTruth) do
        story $ i18n "theDreamEaters.weaverOfTheCosmos.theSpiderQueen2"
        removeCampaignCard Assets.randolphCarterChainedToTheWakingWorld

      when (survived && not knowsTheTruth) do
        story $ i18n "theDreamEaters.weaverOfTheCosmos.theSpiderQueen3"
        record TheInvestigatorsAreTrappedInAtlachNacha'sRealm
        removeCampaignCard Assets.randolphCarterChainedToTheWakingWorld
        difficulty <- scenarioField ScenarioDifficulty
        addChaosToken $ case difficulty of
          Easy -> MinusThree
          Standard -> MinusFour
          Hard -> MinusFive
          Expert -> MinusSeven

      story $ i18n "theDreamEaters.weaverOfTheCosmos.theSpiderQueen4"

      notBottom <- select $ not_ (LocationWithLabel "theGreatWeb4")

      for_ notBottom $ push . RemoveLocation
      top <- selectJust $ LocationWithLabel "theGreatWeb4"
      otherWebs <-
        placeLabeledLocationsFrom "theGreatWeb" 5
          . take 7
          =<< getSetAsideCardsMatching (CardWithTitle "The Great Web")

      pushAll
        [ PlacedLocationDirection l1 Below l2
        | (l1, l2) <- zip (top : otherWebs) otherWebs
        ]

      let locationIds = top :| otherWebs
      push $ PlacedLocationDirection top Above (NE.last locationIds)

      atlachNacha <-
        traverse
          getSetAsideCard
          [ Enemies.atlachNacha
          , Enemies.legsOfAtlachNacha_347
          , Enemies.legsOfAtlachNacha_348
          , Enemies.legsOfAtlachNacha_349
          , Enemies.legsOfAtlachNacha_350
          ]

      case locationIds of
        _ :| [loc2, _, loc4, _, loc3, _, loc1] -> do
          for_ (zip atlachNacha [Global, AtLocation loc1, AtLocation loc2, AtLocation loc3, AtLocation loc4]) $ \(part, placement) ->
            push . toMessage =<< createEnemy part placement
        _ -> error "wrong number of locations"

      push
        $ SetLayout
          [ ".             .             .             .     .     theGreatWeb4 theGreatWeb4 .     .     . . ."
          , ".             .             .             .     .     theGreatWeb4 theGreatWeb4 .     .     . . ."
          , ".             theGreatWeb11 theGreatWeb11 .     .     theGreatWeb4 theGreatWeb4 .     .     theGreatWeb5 theGreatWeb5 ."
          , ".             theGreatWeb11 theGreatWeb11 .     .     theGreatWeb4 theGreatWeb4 .     .     theGreatWeb5 theGreatWeb5 ."
          , ".             theGreatWeb11 theGreatWeb11 .     .     theGreatWeb4 theGreatWeb4 .     .     theGreatWeb5 theGreatWeb5 ."
          , ".             theGreatWeb11 theGreatWeb11 .     .     theGreatWeb4 theGreatWeb4 .     .     theGreatWeb5 theGreatWeb5 ."
          , ".             theGreatWeb11 theGreatWeb11 .     .     .            .            .     .     theGreatWeb5 theGreatWeb5 ."
          , ".             theGreatWeb11 theGreatWeb11 .     .     .            .            .     .     theGreatWeb5 theGreatWeb5 ."
          , ".             .             .             legs1 legs1 .            .            legs2 legs2 .            .            ."
          , ".             .             .             legs1 legs1 .            .            legs2 legs2 .            .            ."
          , ".             .             .             legs1 legs1 .            .            legs2 legs2 .            .            ."
          , "theGreatWeb10 theGreatWeb10 .             legs1 legs1 atlachNacha  atlachNacha  legs2 legs2 .            theGreatWeb6 theGreatWeb6"
          , "theGreatWeb10 theGreatWeb10 .             legs1 legs1 atlachNacha  atlachNacha  legs2 legs2 .            theGreatWeb6 theGreatWeb6"
          , "theGreatWeb10 theGreatWeb10 .             legs1 legs1 atlachNacha  atlachNacha  legs2 legs2 .            theGreatWeb6 theGreatWeb6"
          , "theGreatWeb10 theGreatWeb10 .             legs3 legs3 atlachNacha  atlachNacha  legs4 legs4 .            theGreatWeb6 theGreatWeb6"
          , "theGreatWeb10 theGreatWeb10 .             legs3 legs3 atlachNacha  atlachNacha  legs4 legs4 .            theGreatWeb6 theGreatWeb6"
          , "theGreatWeb10 theGreatWeb10 .             legs3 legs3 atlachNacha  atlachNacha  legs4 legs4 .            theGreatWeb6 theGreatWeb6"
          , ".             .             .             legs3 legs3 .            .            legs4 legs4 .            .            ."
          , ".             .             .             legs3 legs3 .            .            legs4 legs4 .            .            ."
          , ".             .             .             legs3 legs3 .            .            legs4 legs4 .            .            ."
          , ".             theGreatWeb9  theGreatWeb9  .     .     .            .            .     .     theGreatWeb7 theGreatWeb7 ."
          , ".             theGreatWeb9  theGreatWeb9  .     .     .            .            .     .     theGreatWeb7 theGreatWeb7 ."
          , ".             theGreatWeb9  theGreatWeb9  .     .     theGreatWeb8 theGreatWeb8 .     .     theGreatWeb7 theGreatWeb7 ."
          , ".             theGreatWeb9  theGreatWeb9  .     .     theGreatWeb8 theGreatWeb8 .     .     theGreatWeb7 theGreatWeb7 ."
          , ".             theGreatWeb9  theGreatWeb9  .     .     theGreatWeb8 theGreatWeb8 .     .     theGreatWeb7 theGreatWeb7 ."
          , ".             theGreatWeb9  theGreatWeb9  .     .     theGreatWeb8 theGreatWeb8 .     .     theGreatWeb7 theGreatWeb7 ."
          , ".             .             .             .     .     theGreatWeb8 theGreatWeb8 .     .     . . ."
          , ".             .             .             .     .     theGreatWeb8 theGreatWeb8 .     .     . . ."
          ]

      advanceActDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    _ -> JourneyAcrossTheBridge <$> lift (runMessage msg attrs)
