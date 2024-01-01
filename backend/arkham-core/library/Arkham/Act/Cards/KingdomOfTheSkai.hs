module Arkham.Act.Cards.KingdomOfTheSkai (KingdomOfTheSkai (..), kingdomOfTheSkai) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Port))

newtype KingdomOfTheSkai = KingdomOfTheSkai ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- Search the encounter deck, discard pile, and all play areas for Cats of
-- Ulthar, each Pack of Vooniths, and each card from the Zoogs encounter set,
-- and remove them from the game.

kingdomOfTheSkai :: ActCard KingdomOfTheSkai
kingdomOfTheSkai =
  act
    (1, A)
    KingdomOfTheSkai
    Cards.kingdomOfTheSkai
    (Just $ GroupClueCost (PerPlayer 2) (LocationWithTrait Port))

instance RunMessage KingdomOfTheSkai where
  runMessage msg a@(KingdomOfTheSkai attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      (leadId, lead) <- getLeadInvestigatorPlayer
      players <- getAllPlayers
      investigators <- getInvestigators
      priests <- getSetAsideCardsMatching "Priest of a Thousand Masks"

      victoryLocations <- selectList $ LocationWithVictory <> LocationWithoutClues
      locations <- filter (`notElem` victoryLocations) <$> selectList Anywhere

      (baharna, placeBaharna) <- placeSetAsideLocation Locations.baharna
      placeOriabRest <- placeSetAsideLocations [Locations.mtNgranek, Locations.namelessRuins]

      (kadatheron, placeKadatheron) <- placeSetAsideLocation Locations.kadatheron
      (ruinsOfIb, placeRuinsOfIb) <- placeSetAsideLocation Locations.ruinsOfIb
      placeSarnath <- placeSetAsideLocation_ Locations.sarnath
      beingsOfIb <- getSetAsideCard Enemies.beingsOfIb
      createBeingsOfIb <- createEnemyAt_ beingsOfIb ruinsOfIb Nothing

      (ilekVad, placeIlekVad) <- placeSetAsideLocation Locations.ilekVad
      (forbiddenLands, placeForbiddenLands) <- placeSetAsideLocation Locations.forbiddenLands
      (zulanThek, placeZulanThek) <- placeSetAsideLocation Locations.zulanThek
      stalkingManticore <- getSetAsideCard Enemies.stalkingManticore
      hordeOfNight <- getSetAsideCard Enemies.hordeOfNight
      createStalkingManticore <- createEnemyAt_ stalkingManticore forbiddenLands Nothing
      createHordeOfNight <- createEnemyAt_ hordeOfNight zulanThek Nothing

      (celephais, placeCelephais) <- placeSetAsideLocation Locations.celephais
      placeTimlessRealmRest <- placeSetAsideLocations [Locations.serannian, Locations.hazuthKleg]
      theCrawlingMist <- getSetAsideCard Enemies.theCrawlingMist

      pushAll
        [ ShuffleCardsIntoDeck Deck.EncounterDeck priests
        , ShuffleEncounterDiscardBackIn
        , storyWithChooseOne
            lead
            players
            "You find a captain willing to grant you passage to the remote regions of the Dreamlands, wherein you may find signs from the gods to point you in the direction of Kadath."
            [ Label
                "Visit the isle of Oriab to the south. Resolve _Oriab Setup_ in the Campaign Guide."
                $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
                <> [placeBaharna, MoveAllTo (toSource attrs) baharna]
                <> placeOriabRest
                <> map (AddToVictory . toTarget) victoryLocations
                <> map RemoveLocation locations
                <> [ search
                      leadId
                      (toSource attrs)
                      EncounterDeckTarget
                      [fromDeck]
                      (cardIs Enemies.nightriders)
                      (DeferSearchedToTarget $ toTarget attrs)
                   , AdvanceToAct (actDeckId attrs) Acts.theIsleOfOriab A (toSource attrs)
                   ]
            , Label "Visit the ancient land of Mnar to the west. Resolve _Mnar Setup_ in the Campaign Guide."
                $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
                <> [placeKadatheron, MoveAllTo (toSource attrs) kadatheron]
                <> [placeRuinsOfIb, placeSarnath, createBeingsOfIb]
                <> map (AddToVictory . toTarget) victoryLocations
                <> map RemoveLocation locations
                <> [AdvanceToAct (actDeckId attrs) Acts.theDoomThatCameBefore A (toSource attrs)]
            , Label
                "Visit the Forbidden Lands to the north. Resolve _Forbidden Lands Setup_ in the Campaign Guide."
                $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
                <> [placeIlekVad, MoveAllTo (toSource attrs) ilekVad]
                <> [placeForbiddenLands, placeZulanThek, createStalkingManticore, createHordeOfNight]
                <> map (AddToVictory . toTarget) victoryLocations
                <> map RemoveLocation locations
                <> [AdvanceToAct (actDeckId attrs) Acts.seekOutTheNight A (toSource attrs)]
            , Label
                "Visit the kingdom of the Timeless Realm to the east. Resolve _Timeless Realm Setup_ in the Campaign Guide."
                $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
                <> [placeCelephais, MoveAllTo (toSource attrs) celephais]
                <> placeTimlessRealmRest
                <> map (AddToVictory . toTarget) victoryLocations
                <> map RemoveLocation locations
                <> [ ShuffleCardsIntoDeck Deck.EncounterDeck [theCrawlingMist]
                   , search
                      leadId
                      (toSource attrs)
                      EncounterDeckTarget
                      [fromDeck]
                      (cardIs Enemies.priestOfAThousandMasks)
                      (DeferSearchedToTarget $ toTarget attrs)
                   , AdvanceToAct (actDeckId attrs) Acts.theKingsDecree A (toSource attrs)
                   ]
            ]
        ]
      pure a
    SearchFound _ (isTarget attrs -> True) _ cards | notNull cards -> do
      n <- getPlayerCount
      case cards of
        (x : _) | cardMatch x (cardIs Enemies.nightriders) -> do
          let nightriders = take (if n >= 3 then 2 else 1) cards
          when (notNull nightriders) $ do
            mtNgranek <- selectJust $ locationIs Locations.mtNgranek
            namelessRuins <- selectJust $ locationIs Locations.namelessRuins
            pushAllM
              $ traverse (\(c, l) -> createEnemyAt_ c l Nothing)
              $ zip nightriders [mtNgranek, namelessRuins]
        (x : _) | cardMatch x (cardIs Enemies.priestOfAThousandMasks) -> do
          let priests = take (if n >= 3 then 2 else 1) cards
          when (notNull priests) $ do
            hazuthKleg <- selectJust $ locationIs Locations.hazuthKleg
            celephais <- selectJust $ locationIs Locations.celephais
            pushAllM
              $ traverse (\(c, l) -> createEnemyAt_ c l Nothing)
              $ zip priests [hazuthKleg, celephais]
        _ -> error "Not possible"
      pure a
    _ -> KingdomOfTheSkai <$> runMessage msg attrs
