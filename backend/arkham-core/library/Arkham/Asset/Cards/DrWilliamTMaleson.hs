module Arkham.Asset.Cards.DrWilliamTMaleson (drWilliamTMaleson, DrWilliamTMaleson (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window

newtype DrWilliamTMaleson = DrWilliamTMaleson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drWilliamTMaleson :: AssetCard DrWilliamTMaleson
drWilliamTMaleson = ally DrWilliamTMaleson Cards.drWilliamTMaleson (2, 2)

instance HasAbilities DrWilliamTMaleson where
  getAbilities (DrWilliamTMaleson attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
          (DrawCard #when You (basic IsEncounterCard) (DeckOf You))
        $ Costs [ExhaustCost (toTarget attrs), PlaceClueOnLocationCost 1]
    ]

dropUntilDraw :: [Message] -> [Message]
dropUntilDraw = dropWhile (notElem DrawEncounterCardMessage . messageType)

instance RunMessage DrWilliamTMaleson where
  runMessage msg a@(DrWilliamTMaleson attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ignoreWindow <-
        checkWindows
          [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect $ attrs.ability 1)]
      card <- withQueue $ \queue -> case dropUntilDraw queue of
        InvestigatorDrewEncounterCard _ card' : queue' -> (queue', card')
        _ -> error "unhandled"
      key <- getEncounterDeckKey (toCardId card)
      pushAll
        [ ShuffleCardsIntoDeck (Deck.EncounterDeckByKey key) [EncounterCard card]
        , drawEncounterCard iid attrs
        , ignoreWindow
        ]
      pure a
    _ -> DrWilliamTMaleson <$> runMessage msg attrs
