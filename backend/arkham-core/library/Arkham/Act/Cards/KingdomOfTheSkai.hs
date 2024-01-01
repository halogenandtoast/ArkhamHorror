module Arkham.Act.Cards.KingdomOfTheSkai (KingdomOfTheSkai (..), kingdomOfTheSkai) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet (EncounterSet (Zoogs))
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.TheSearchForKadath.Helpers
import Arkham.Trait (Trait (Port))

newtype KingdomOfTheSkai = KingdomOfTheSkai ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

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
      lead <- getLeadPlayer
      players <- getAllPlayers
      priests <- getSetAsideCardsMatching "Priest of a Thousand Masks"

      pushAll
        [ RemoveAllCopiesOfEncounterCardFromGame
            $ oneOf [cardIs Enemies.catsOfUlthar, cardIs Enemies.packOfVooniths, CardFromEncounterSet Zoogs]
        , ShuffleCardsIntoDeck Deck.EncounterDeck priests
        , ShuffleEncounterDiscardBackIn
        , storyWithChooseOne
            lead
            players
            "You find a captain willing to grant you passage to the remote regions of the Dreamlands, wherein you may find signs from the gods to point you in the direction of Kadath."
            [ Label
                "Visit the isle of Oriab to the south. Resolve _Oriab Setup_ in the Campaign Guide."
                [SetScenarioMeta $ toJSON Oriab]
            , Label
                "Visit the ancient land of Mnar to the west. Resolve _Mnar Setup_ in the Campaign Guide."
                [SetScenarioMeta $ toJSON Mnar]
            , Label
                "Visit the Forbidden Lands to the north. Resolve _Forbidden Lands Setup_ in the Campaign Guide."
                [SetScenarioMeta $ toJSON ForbiddenLands]
            , Label
                "Visit the kingdom of the Timeless Realm to the east. Resolve _Timeless Realm Setup_ in the Campaign Guide."
                [SetScenarioMeta $ toJSON TimelessRealm]
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
