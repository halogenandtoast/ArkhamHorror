module Arkham.Types.Scenario.Scenarios.APhantomOfTruth
  ( APhantomOfTruth(..)
  , aPhantomOfTruth
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.APhantomOfTruth.Story
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet qualified as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token

newtype APhantomOfTruth = APhantomOfTruth ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aPhantomOfTruth :: Difficulty -> APhantomOfTruth
aPhantomOfTruth difficulty =
  APhantomOfTruth
    $ baseAttrs "03200" "A Phantom of Truth" difficulty
    & locationLayoutL
    ?~ [ "grandGuignol .                   canalSaintMartin ."
       , "grandGuignol montmartre           canalSaintMartin péreLachaiseCemetery"
       , "opéraGarnier montmartre           leMarais         péreLachaiseCemetery"
       , "opéraGarnier .                   leMarais         ."
       , "gareDOrsay   gardensOfLuxembourg notreDame        ."
       , "gareDOrsay   gardensOfLuxembourg notreDame        ."
       , ".            montparnasse        .                ."
       , ".            montparnasse        .                ."
       ]

instance HasRecord env APhantomOfTruth where
  hasRecord _ _ = pure False
  hasRecordSet _ _ = pure []
  hasRecordCount _ _ = pure 0

instance HasTokenValue env InvestigatorId => HasTokenValue env APhantomOfTruth where
  getTokenValue (APhantomOfTruth attrs) iid = \case
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
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
  , Skull
  , AutoFail
  , ElderSign
  ]

gatherTheMidnightMasks :: MonadRandom m => Int -> Int -> m [EncounterCard]
gatherTheMidnightMasks conviction doubt = traverse
  genEncounterCard
  (if conviction > doubt
    then
      [ Treacheries.huntingShadow
      , Treacheries.huntingShadow
      , Treacheries.huntingShadow
      ]
    else [Treacheries.falseLead, Treacheries.falseLead]
  )

instance ScenarioRunner env => RunMessage env APhantomOfTruth where
  runMessage msg s@(APhantomOfTruth attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ do
        randomToken <- sample (Cultist :| [Tablet, ElderThing])
        push (SetTokens $ standaloneTokens <> [randomToken, randomToken])
      pure s
    Setup -> do

      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt

      let
        act1 = if conviction > doubt
          then Acts.theParisianConspiracyV2
          else Acts.theParisianConspiracyV1
        excludes = if conviction > doubt
          then [Treacheries.blackStarsRise]
          else [Treacheries.twinSuns]

      gatheredCards <- buildEncounterDeckExcluding
        excludes
        [ EncounterSet.APhantomOfTruth
        , EncounterSet.EvilPortents
        , EncounterSet.Byakhee
        , EncounterSet.TheStranger
        , EncounterSet.AgentsOfHastur
        ]
      midnightMasks <- gatherTheMidnightMasks conviction doubt
      encounterDeck <- Deck <$> shuffleM (unDeck gatheredCards <> midnightMasks)

      setAsideCards <- traverse genCard [Enemies.theOrganistHopelessIDefiedHim]

      montmartre <- genCard
        =<< sample (Locations.montmartre209 :| [Locations.montmartre210])

      operaGarnier <- genCard
        =<< sample (Locations.operaGarnier212 :| [Locations.operaGarnier213])

      leMarais <- genCard
        =<< sample (Locations.leMarais217 :| [Locations.leMarais218])

      montparnasse <- genCard Locations.montparnasse
      grandGuignol <- genCard Locations.grandGuignol
      gareDOrsay <- genCard Locations.gareDOrsay
      pereLachaiseCemetery <- genCard Locations.pereLachaiseCemetery
      canalSaintMartin <- genCard Locations.canalSaintMartin
      notreDame <- genCard Locations.notreDame
      gardensOfLuxembourg <- genCard Locations.gardensOfLuxembourg

      investigatorIds <- getInvestigatorIds

      pushAll
        ([story investigatorIds intro1]
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , PlaceLocation montmartre
           , PlaceLocation operaGarnier
           , PlaceLocation leMarais
           , PlaceLocation montparnasse
           , PlaceLocation gareDOrsay
           , PlaceLocation grandGuignol
           , PlaceLocation canalSaintMartin
           , PlaceLocation pereLachaiseCemetery
           , PlaceLocation notreDame
           , PlaceLocation gardensOfLuxembourg
           , MoveAllTo (toSource attrs) (toLocationId gareDOrsay)
           ]
        )

      APhantomOfTruth <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [act1, Acts.pursuingShadows, Acts.stalkedByShadows]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.theFirstNight
             , Agendas.theSecondNight
             , Agendas.theThirdNight
             ]
          )
        )
    _ -> APhantomOfTruth <$> runMessage msg attrs
