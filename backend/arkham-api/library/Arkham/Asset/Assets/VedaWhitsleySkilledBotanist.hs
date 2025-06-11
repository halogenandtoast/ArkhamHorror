module Arkham.Asset.Assets.VedaWhitsleySkilledBotanist (vedaWhitsleySkilledBotanist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Strategy

newtype VedaWhitsleySkilledBotanist = VedaWhitsleySkilledBotanist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Exhaust Veda Whitsley: Look at the top card of the exploration deck or encounter deck. If it is an enemy, you may deal 1 damage to Veda Whitsley to discard it. If it is a treachery, you may deal 1 horror to Veda Whitsley to discard it. Otherwise, return it to the top of its deck.

vedaWhitsleySkilledBotanist :: AssetCard VedaWhitsleySkilledBotanist
vedaWhitsleySkilledBotanist = ally VedaWhitsleySkilledBotanist Cards.vedaWhitsleySkilledBotanist (3, 3)

instance HasAbilities VedaWhitsleySkilledBotanist where
  getAbilities (VedaWhitsleySkilledBotanist a) =
    [restricted a 1 (ControlsThis <> youExist can.target.encounterDeck) $ FastAbility (exhaust a)]

instance RunMessage VedaWhitsleySkilledBotanist where
  runMessage msg a@(VedaWhitsleySkilledBotanist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasEncounterDeck <- can.target.encounterDeck iid
      explorationDeck <- getExplorationDeck
      chooseOrRunOneM iid $ scenarioI18n do
        when (notNull explorationDeck) do
          labeled' "vedaWhitsley.exploration"
            $ lookAt
              iid
              (attrs.ability 1)
              ScenarioDeckTarget
              [(FromTopOfDeck 1, PutBack)]
              (basic AnyCard)
              (defer attrs IsNotDraw)
        when hasEncounterDeck do
          labeled' "vedaWhitsley.encounter"
            $ lookAt
              iid
              (attrs.ability 1)
              EncounterDeckTarget
              [(FromTopOfDeck 1, PutBack)]
              (basic AnyCard)
              (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck (c : _) -> do
      focusCards [c] do
        if
          | cardMatch c (CardWithType EnemyType) ->
              chooseOneM iid $ scenarioI18n do
                labeled' "vedaWhitsley.damage" do
                  dealAssetDamage attrs.id (attrs.ability 1) 1
                  discardCard iid (attrs.ability 1) c
                labeled' "vedaWhitsley.putBack" nothing
          | cardMatch c (CardWithType TreacheryType) ->
              chooseOneM iid $ scenarioI18n do
                labeled' "vedaWhitsley.horror" do
                  dealAssetHorror attrs.id (attrs.ability 1) 1
                  discardCard iid (attrs.ability 1) c
                labeled' "vedaWhitsley.putBack" nothing
          | otherwise -> continue_ iid
      pure a
    _ -> VedaWhitsleySkilledBotanist <$> liftRunMessage msg attrs
