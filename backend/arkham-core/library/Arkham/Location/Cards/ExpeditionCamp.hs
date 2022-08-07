module Arkham.Location.Cards.ExpeditionCamp
  ( expeditionCamp
  , ExpeditionCamp(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.Target

newtype ExpeditionCamp = ExpeditionCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionCamp :: LocationCard ExpeditionCamp
expeditionCamp = location ExpeditionCamp Cards.expeditionCamp 1 (Static 0)

instance HasAbilities ExpeditionCamp where
  getAbilities (ExpeditionCamp attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ withTooltip
          "The wilds are too dangerous!"
          (locationResignAction attrs)
        , restrictedAbility
          attrs
          2
          (Here <> HasSupply Map)
          (ActionAbility Nothing $ ActionCost 1)
        ]
      else []

instance RunMessage ExpeditionCamp where
  runMessage msg l@(ExpeditionCamp attrs) = case msg of
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      explorationDeck <- getExplorationDeck
      let
        (viewing, rest) = splitAt 3 explorationDeck
        cardPairs = map (toSnd (`deleteFirst` viewing)) viewing
      pushAll
        [ FocusCards viewing
        , SetScenarioDeck ExplorationDeck rest
        , Ask iid
        $ QuestionLabel "Place one card on bottom of exploration deck"
        $ ChooseOne
            [ TargetLabel
                (CardIdTarget $ toCardId c)
                [ PutCardOnBottomOfDeck
                  iid
                  (Deck.ScenarioDeckByKey ExplorationDeck)
                  c
                , FocusCards remaining
                , Ask iid
                $ QuestionLabel "Place card on top of exploration deck"
                $ ChooseOneAtATime
                    [ TargetLabel
                        (CardIdTarget $ toCardId r)
                        [ PutCardOnTopOfDeck
                            iid
                            (Deck.ScenarioDeckByKey ExplorationDeck)
                            r
                        ]
                    | r <- remaining
                    ]
                ]
            | (c, remaining) <- cardPairs
            ]
        , UnfocusCards
        ]
      pure l
    _ -> ExpeditionCamp <$> runMessage msg attrs
