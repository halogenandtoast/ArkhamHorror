module Arkham.Asset.Cards.DrWilliamTMaleson
  ( drWilliamTMaleson
  , DrWilliamTMaleson(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype DrWilliamTMaleson = DrWilliamTMaleson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drWilliamTMaleson :: AssetCard DrWilliamTMaleson
drWilliamTMaleson = ally DrWilliamTMaleson Cards.drWilliamTMaleson (2, 2)

instance HasAbilities DrWilliamTMaleson where
  getAbilities (DrWilliamTMaleson attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
            (DrawCard
              Timing.When
              You
              (BasicCardMatch IsEncounterCard)
              (DeckOf You)
            )
        $ Costs [ExhaustCost (toTarget attrs), PlaceClueOnLocationCost 1]
    ]

dropUntilDraw :: [Message] -> [Message]
dropUntilDraw = dropWhile (notElem DrawEncounterCardMessage . messageType)

instance RunMessage DrWilliamTMaleson where
  runMessage msg a@(DrWilliamTMaleson attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      card <- withQueue $ \queue -> case dropUntilDraw queue of
        InvestigatorDrewEncounterCard _ card' : queue' -> (queue', card')
        _ -> error "unhandled"
      a <$ pushAll
        [ ShuffleCardsIntoDeck Deck.EncounterDeck [EncounterCard card]
        , InvestigatorDrawEncounterCard iid
        ]
    _ -> DrWilliamTMaleson <$> runMessage msg attrs
