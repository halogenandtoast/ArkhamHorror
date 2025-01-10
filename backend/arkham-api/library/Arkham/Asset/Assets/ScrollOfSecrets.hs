module Arkham.Asset.Assets.ScrollOfSecrets (scrollOfSecrets) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Taboo

newtype ScrollOfSecrets = ScrollOfSecrets AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfSecrets :: AssetCard ScrollOfSecrets
scrollOfSecrets = asset ScrollOfSecrets Cards.scrollOfSecrets

instance HasAbilities ScrollOfSecrets where
  getAbilities (ScrollOfSecrets a) =
    [ controlled a 1 (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ (if tabooed TabooList18 a then FastAbility else actionAbilityWithCost)
        $ exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage ScrollOfSecrets where
  runMessage msg a@(ScrollOfSecrets attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <- selectTargets $ affectsOthers can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      chooseTargetM iid ([EncounterDeckTarget | hasEncounterDeck] <> xs) \target -> do
        lookAt iid attrs target [(FromBottomOfDeck 1, DoNothing)] #any (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      focusCards cards do
        chooseOrRunOneM iid do
          targets (onlyEncounterCards cards) \card ->
            chooseOneM iid do
              labeled "Discard" $ discard card
              labeled "Place on bottom of encounter deck" $ putCardOnBottomOfDeck iid Deck.EncounterDeck card
              labeled "Place on top of encounter deck" $ putCardOnTopOfDeck iid Deck.EncounterDeck card
      pure a
    SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') cards -> do
      focusCards cards do
        chooseOrRunOneM iid do
          targets (onlyPlayerCards cards) \card -> do
            chooseOneM iid do
              labeled "Discard" $ discard card
              labeled "Add to Hand" $ addToHand iid' (only card)
              labeled "Place on bottom of deck" $ putCardOnBottomOfDeck iid deck card
              labeled "Place on top of deck" $ putCardOnTopOfDeck iid deck card
      pure a
    _ -> ScrollOfSecrets <$> liftRunMessage msg attrs
