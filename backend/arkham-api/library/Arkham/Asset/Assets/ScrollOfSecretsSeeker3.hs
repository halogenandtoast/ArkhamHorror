module Arkham.Asset.Assets.ScrollOfSecretsSeeker3 (scrollOfSecretsSeeker3) where

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

newtype ScrollOfSecretsSeeker3 = ScrollOfSecretsSeeker3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfSecretsSeeker3 :: AssetCard ScrollOfSecretsSeeker3
scrollOfSecretsSeeker3 =
  asset ScrollOfSecretsSeeker3 Cards.scrollOfSecretsSeeker3

instance HasAbilities ScrollOfSecretsSeeker3 where
  getAbilities (ScrollOfSecretsSeeker3 a) =
    [ controlled a 1 (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ (if tabooed TabooList18 a then FastAbility else actionAbilityWithCost)
        $ exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage ScrollOfSecretsSeeker3 where
  runMessage msg a@(ScrollOfSecretsSeeker3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <- selectTargets $ affectsOthers can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      chooseTargetM iid ([EncounterDeckTarget | hasEncounterDeck] <> xs) \target -> do
        lookAt iid attrs target [(FromBottomOfDeck 3, DoNothing)] #any (defer attrs IsNotDraw)
      pure a
    SearchFound _ (isTarget attrs -> True) _ cards | notNull cards -> do
      doStep 1 msg
      pure a
    DoStep 1 msg'@(SearchFound iid (isTarget attrs -> True) deck cards) | notNull cards -> do
      focusCards cards do
        chooseOneM iid do
          questionLabeled "Discard 1 card?"
          labeled "Do not discard" $ unfocusCards >> doStep 2 msg'
          targets cards \card -> do
            unfocusCards
            addToDiscard iid (only card)
            doStep 2 $ SearchFound iid (toTarget attrs) deck (deleteFirst card cards)
      pure a
    DoStep 2 msg'@(SearchFound _ (isTarget attrs -> True) Deck.EncounterDeck _) -> do
      doStep 3 msg'
      pure a
    DoStep 2 msg'@(SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') cards) | notNull cards -> do
      let playerCards = onlyPlayerCards cards
      if null playerCards
        then doStep 3 msg'
        else focusCards cards do
          chooseOneM iid do
            questionLabeled "Add 1 card to hand?"
            labeled "Do not add to hand" $ unfocusCards >> doStep 3 msg'
            targets playerCards \card -> do
              unfocusCards
              addToHand iid' (only card)
              doStep 3 $ SearchFound iid (toTarget attrs) deck (deleteFirst (PlayerCard card) cards)
      pure a
    DoStep 3 (SearchFound iid (isTarget attrs -> True) deck cards) | notNull cards -> do
      focusCards cards do
        chooseOrRunOneM iid do
          targets cards \card -> do
            chooseOneM iid do
              labeled "Place on bottom of deck" do
                unfocusCards
                focusCard card do
                  putCardOnBottomOfDeck iid deck card
                  doStep 3 $ SearchFound iid (toTarget attrs) deck (deleteFirst card cards)
              labeled "Place on top of deck" do
                unfocusCards
                focusCard card do
                  putCardOnTopOfDeck iid deck card
                  doStep 3 $ SearchFound iid (toTarget attrs) deck (deleteFirst card cards)
      pure a
    _ -> ScrollOfSecretsSeeker3 <$> liftRunMessage msg attrs
