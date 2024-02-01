module Arkham.Asset.Cards.DrWilliamTMaleson (
  drWilliamTMaleson,
  DrWilliamTMaleson (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype DrWilliamTMaleson = DrWilliamTMaleson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

drWilliamTMaleson :: AssetCard DrWilliamTMaleson
drWilliamTMaleson = ally DrWilliamTMaleson Cards.drWilliamTMaleson (2, 2)

instance HasAbilities DrWilliamTMaleson where
  getAbilities (DrWilliamTMaleson attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
          ( DrawCard
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
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      ignoreWindow <-
        checkWindows
          [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toAbilitySource attrs 1)]
      card <- withQueue $ \queue -> case dropUntilDraw queue of
        InvestigatorDrewEncounterCard _ card' : queue' -> (queue', card')
        _ -> error "unhandled"
      key <- getEncounterDeckKey (toCardId card)
      pushAll
        [ ShuffleCardsIntoDeck (Deck.EncounterDeckByKey key) [EncounterCard card]
        , InvestigatorDrawEncounterCard iid
        , ignoreWindow
        ]
      pure a
    _ -> DrWilliamTMaleson <$> runMessage msg attrs
