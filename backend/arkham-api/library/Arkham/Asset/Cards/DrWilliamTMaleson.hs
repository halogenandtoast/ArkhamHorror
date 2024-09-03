module Arkham.Asset.Cards.DrWilliamTMaleson (drWilliamTMaleson, DrWilliamTMaleson (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario (getEncounterDeckKey)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher

newtype DrWilliamTMaleson = DrWilliamTMaleson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drWilliamTMaleson :: AssetCard DrWilliamTMaleson
drWilliamTMaleson = ally DrWilliamTMaleson Cards.drWilliamTMaleson (2, 2)

instance HasAbilities DrWilliamTMaleson where
  getAbilities (DrWilliamTMaleson attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
          (DrawCard #when You (basic IsEncounterCard) EncounterDeck)
          (exhaust attrs <> PlaceClueOnLocationCost 1)
    ]

instance RunMessage DrWilliamTMaleson where
  runMessage msg a@(DrWilliamTMaleson attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      quietCancelCardDraw card
      deck <- Deck.EncounterDeckByKey <$> getEncounterDeckKey card.id
      push $ ShuffleCardsIntoDeck deck [card]
      drawEncounterCard iid attrs
      cancelledOrIgnoredCardOrGameEffect $ attrs.ability 1
      pure a
    _ -> DrWilliamTMaleson <$> liftRunMessage msg attrs
