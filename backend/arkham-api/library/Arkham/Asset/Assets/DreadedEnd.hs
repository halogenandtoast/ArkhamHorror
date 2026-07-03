module Arkham.Asset.Assets.DreadedEnd (dreadedEnd) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card (onlyEncounterCards, toCard)
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers (scenarioI18n)
import Arkham.Strategy

newtype DreadedEnd = DreadedEnd AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadedEnd :: AssetCard DreadedEnd
dreadedEnd = asset DreadedEnd Cards.dreadedEnd

instance HasAbilities DreadedEnd where
  getAbilities (DreadedEnd a) =
    [controlled_ a 1 $ actionAbilityWithCost (assetUseCost a Charge 1)]

instance RunMessage DreadedEnd where
  runMessage msg a@(DreadedEnd attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NonEliteEnemy <> enemyAtLocationWith iid
      scenarioI18n $ chooseOrRunOneM iid do
        labeledValidate' (notNull enemies) "placeEnemyOnTopOfEncounterDeck" do
          chooseTargetM iid enemies $ putOnTopOfDeck iid Deck.EncounterDeck
        labeled' "lookAtTopOfEncounterDeck" do
          lookAt iid (attrs.ability 1) EncounterDeckTarget [(FromTopOfDeck 5, PutBackInAnyOrder)] #any
            $ defer attrs IsNotDraw
      pure a
    SearchFound iid (isTarget attrs -> True) _ (onlyEncounterCards -> cards) | notNull cards -> do
      focusCards (map toCard cards) do
        chooseNM iid (min 2 (length cards)) $ targets cards \card -> do
          obtainCard card
          addToEncounterDiscard (only card)
      pure a
    _ -> DreadedEnd <$> liftRunMessage msg attrs
