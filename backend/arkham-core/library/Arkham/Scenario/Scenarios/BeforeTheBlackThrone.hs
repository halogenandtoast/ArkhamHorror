module Arkham.Scenario.Scenarios.BeforeTheBlackThrone (
  BeforeTheBlackThrone (..),
  beforeTheBlackThrone,
) where

import Arkham.Prelude hiding ((<|))

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Label
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Scenarios.BeforeTheBlackThrone.Story
import Data.Aeson (Result (..))

-- * Cosmos

{- $cosmos
The cosmos is an internal data structure that represents a grid which can
grow in any direction. Additionally, we need to track if there is empty
space or a card at a specific position. Finally, cards can slide around.
Because this logic needs to be known by locations we store in the
`scenarioMeta` field. That way it can be passed around and set accordingly.
-}

newtype BeforeTheBlackThrone = BeforeTheBlackThrone ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beforeTheBlackThrone :: Difficulty -> BeforeTheBlackThrone
beforeTheBlackThrone difficulty =
  scenario
    BeforeTheBlackThrone
    "05325"
    "Before the Black Throne"
    difficulty
    []

instance HasChaosTokenValue BeforeTheBlackThrone where
  getChaosTokenValue iid tokenFace (BeforeTheBlackThrone attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

-- Æ Find the Hideous Palace, Court of the Great Old Ones, and The Black Throne locations. (They are on the revealed side of 3 of the “Cosmos” locations.) Set those locations aside, out of play.
-- Æ Shuffle the remaining location cards into a separate deck, Cosmos‐side faceup. This deck is called the Cosmos (see “The Cosmos,” below).
-- Æ Take the set‐aside Hideous Palace and the top card of the Cosmos, and shuffle them so you cannot tell which is which. Then, put them into play along with facedown player cards from the top of the lead investigator’s deck, as depicted in “Location Placement for Setup / Act 1.” Facedown player cards represent empty space (see “Empty Space” on the next page).
-- Æ Set the Piper of Azathoth enemy aside, out of play.
-- Æ Put Azathoth into play next to the agenda deck. For the remainder of the scenario, Azathoth is in play, but is not at any location.
-- Æ Check Campaign Log. For each tally mark recorded next to the path winds before you, place 1 resource on the scenario reference card.
-- Æ Shuffle the remainder of the encounter cards to build the encounter deck.
--
--
instance RunMessage BeforeTheBlackThrone where
  runMessage msg s@(BeforeTheBlackThrone attrs) = case msg of
    PreScenarioSetup -> do
      investigators <- allInvestigators
      pushAll [story investigators intro]
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.piperOfAzathoth, Enemies.azathoth]
          [ EncounterSet.BeforeTheBlackThrone
          , EncounterSet.AgentsOfAzathoth
          , EncounterSet.InexorableFate
          , EncounterSet.AncientEvils
          , EncounterSet.DarkCult
          ]
      (cosmicIngress, placeCosmicIngress) <- placeLocationCard Locations.cosmicIngress

      cosmosCards' <-
        shuffleM
          =<< genCards
            [ Locations.infinityOfDarkness
            , Locations.infinityOfDarkness
            , Locations.infinityOfDarkness
            , Locations.cosmicGate
            , Locations.pathwayIntoVoid
            , Locations.pathwayIntoVoid
            , Locations.dancersMist
            , Locations.dancersMist
            , Locations.dancersMist
            , Locations.flightIntoOblivion
            ]

      let
        (topCosmosCard, cosmosCards) =
          case cosmosCards' of
            (x : xs) -> (x, xs)
            _ -> error "did not have enough cards"

      hideousPalace <- genCard Locations.hideousPalace

      (firstCosmosCard, secondCosmosCard) <-
        shuffleM [topCosmosCard, hideousPalace] <&> \case
          [x, y] -> (x, y)
          _ -> error "did not have enough cards"

      lead <- getLead
      (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 6) lead

      (firstCosmos, placeFirstCosmos) <- placeLocation firstCosmosCard
      (secondCosmos, placeSecondCosmos) <- placeLocation secondCosmosCard

      let
        emptySpaceLocations =
          [ Pos 0 1
          , Pos 0 (-1)
          , Pos 1 1
          , Pos 1 0
          , Pos 1 (-1)
          , Pos 2 0
          ]
        emptySpaces = zip emptySpaceLocations cards

      let
        cosmos =
          foldr
            (insertCosmos . uncurry EmptySpace)
            ( insertCosmos (CosmosLocation (Pos 2 1) firstCosmos)
                $ insertCosmos (CosmosLocation (Pos 2 (-1)) secondCosmos)
                $ insertCosmos (CosmosLocation (Pos 0 0) cosmicIngress) initCosmos
            )
            emptySpaces

      placeEmptySpaces <- concatForM emptySpaces $ \(pos, _) -> do
        (emptySpace', placeEmptySpace) <- placeLocationCard Locations.emptySpace
        pure [placeEmptySpace, SetLocationLabel emptySpace' (cosmicLabel pos)]

      azathoth <- genCard Enemies.azathoth
      createAzathoth <- toMessage <$> createEnemy azathoth Global

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetActDeck
          , SetAgendaDeck
          , placeCosmicIngress
          , SetLocationLabel cosmicIngress (cosmicLabel (Pos 0 0))
          , placeFirstCosmos
          , SetLocationLabel firstCosmos (cosmicLabel (Pos 2 1))
          , placeSecondCosmos
          , SetLocationLabel secondCosmos (cosmicLabel (Pos 2 (-1)))
          , MoveAllTo (toSource attrs) cosmicIngress
          , createAzathoth
          ]
          <> map (ObtainCard . toCard) cards
          <> placeEmptySpaces

      agendas <- genCards [Agendas.wheelOfFortuneX, Agendas.itAwaits, Agendas.theFinalCountdown]
      acts <- genCards [Acts.theCosmosBeckons, Acts.inAzathothsDomain, Acts.whatMustBeDone]

      BeforeTheBlackThrone
        <$> runMessage
          msg
          ( attrs
              & (decksL . at CosmosDeck ?~ cosmosCards)
              & locationLayoutL .~ cosmosToGrid cosmos
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
              & (metaL .~ toJSON cosmos)
              & (usesGridL .~ True)
          )
    SetScenarioMeta meta -> do
      case fromJSON @(Cosmos Card LocationId) meta of
        Error err -> error err
        Success cosmos -> pure $ BeforeTheBlackThrone $ attrs & metaL .~ meta & locationLayoutL .~ cosmosToGrid cosmos
    PlaceCosmos _ lid x y -> do
      cosmos' <- getCosmos
      let
        pos = Pos x y
        current = viewCosmos pos cosmos'
        cosmos'' = insertCosmos (CosmosLocation pos lid) cosmos'
      currentMsgs <- case current of
        Just (EmptySpace _ c) -> case toCardOwner c of
          Nothing -> error "Unhandled"
          Just iid -> do
            emptySpace <- selectJust $ FindEmptySpace $ LocationWithLabel (mkLabel $ cosmicLabel pos)
            pure [RemoveFromGame (toTarget emptySpace), ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [c]]
        _ -> pure []
      pushAll
        $ currentMsgs
          <> [ SetLocationLabel lid (cosmicLabel pos)
             , SetScenarioMeta (toJSON cosmos'')
             ]

      pure s
    _ -> BeforeTheBlackThrone <$> runMessage msg attrs
