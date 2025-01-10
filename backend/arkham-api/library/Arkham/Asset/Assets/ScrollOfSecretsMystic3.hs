module Arkham.Asset.Assets.ScrollOfSecretsMystic3 (scrollOfSecretsMystic3) where

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

newtype ScrollOfSecretsMystic3 = ScrollOfSecretsMystic3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfSecretsMystic3 :: AssetCard ScrollOfSecretsMystic3
scrollOfSecretsMystic3 = asset ScrollOfSecretsMystic3 Cards.scrollOfSecretsMystic3

instance HasAbilities ScrollOfSecretsMystic3 where
  getAbilities (ScrollOfSecretsMystic3 a) =
    [ controlled a 1 (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ (if tabooed TabooList18 a then FastAbility else actionAbilityWithCost)
        $ exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage ScrollOfSecretsMystic3 where
  runMessage msg a@(ScrollOfSecretsMystic3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <- selectTargets $ affectsOthers can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      let doSearch target x = lookAt iid attrs target [x 1] #any (defer attrs IsNotDraw)
      chooseTargetM iid ([EncounterDeckTarget | hasEncounterDeck] <> xs) \target -> do
        chooseOneM iid do
          labeled "Look at top" $ doSearch target fromTopOfDeck
          labeled "Look at bottom" $ doSearch target fromBottomOfDeck
      pure a
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      focusCards cards do
        chooseOrRunOneM iid do
          targets (onlyEncounterCards cards) \card ->
            chooseOneM iid do
              labeled "Discard" $ push $ AddToEncounterDiscard card
              labeled "Place on bottom of encounter deck" $ putCardOnBottomOfDeck iid Deck.EncounterDeck card
              labeled "Place on top of encounter deck" $ putCardOnTopOfDeck iid Deck.EncounterDeck card
      pure a
    SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') cards -> do
      focusCards cards do
        chooseOrRunOneM iid do
          targets (onlyPlayerCards cards) \card ->
            chooseOneM iid do
              labeled "Discard" $ push $ AddToDiscard iid' card
              labeled "Add to Hand" $ addToHand iid' (only card)
              labeled "Place on bottom of deck" $ putCardOnBottomOfDeck iid deck card
              labeled "Place on top of deck" $ putCardOnTopOfDeck iid deck card
      pure a
    _ -> ScrollOfSecretsMystic3 <$> liftRunMessage msg attrs
