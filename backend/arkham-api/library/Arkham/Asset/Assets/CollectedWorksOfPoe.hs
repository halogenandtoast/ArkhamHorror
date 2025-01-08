module Arkham.Asset.Assets.CollectedWorksOfPoe (collectedWorksOfPoe) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Strategy

newtype CollectedWorksOfPoe = CollectedWorksOfPoe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collectedWorksOfPoe :: AssetCard CollectedWorksOfPoe
collectedWorksOfPoe = asset CollectedWorksOfPoe Cards.collectedWorksOfPoe

instance HasAbilities CollectedWorksOfPoe where
  getAbilities (CollectedWorksOfPoe a) = [restricted a 1 ControlsThis $ actionAbilityWithCost (assetUseCost a Charge 1)]

instance RunMessage CollectedWorksOfPoe where
  runMessage msg a@(CollectedWorksOfPoe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ affectsOthers $ colocatedWith iid <> can.search.deck
      chooseTargetM iid investigators \iid' -> do
        search
          iid'
          (attrs.ability 1)
          iid'
          [fromTopOfDeck 6]
          (basic $ CardFromEncounterSet Tekelili)
          (defer attrs IsNotDraw)
        shuffleDeck iid'
        drawCardsIfCan iid' (attrs.ability 1) 1
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      focusCards cards \unfocus -> do
        chooseOneAtATimeM iid do
          targets cards $ putCardOnBottomOfDeck iid TekeliliDeck
        push unfocus
      pure a
    _ -> CollectedWorksOfPoe <$> liftRunMessage msg attrs
