module Arkham.Asset.Assets.HallowedMirror3 (hallowedMirror3, HallowedMirror3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (getCanShuffleDeck, searchBonded)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Strategy

newtype HallowedMirror3 = HallowedMirror3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallowedMirror3 :: AssetCard HallowedMirror3
hallowedMirror3 = asset HallowedMirror3 Cards.hallowedMirror3

instance HasAbilities HallowedMirror3 where
  getAbilities (HallowedMirror3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ Matcher.PlayCard #when You (basic $ cardIs Events.soothingMelody)
    ]

instance RunMessage HallowedMirror3 where
  runMessage msg a@(HallowedMirror3 attrs) = runQueueT $ case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      bonded <- take 3 <$> searchBonded iid Events.soothingMelody
      case bonded of
        [] -> pure ()
        (handSoothingMelody : deckSoothingMelodies) -> do
          addToHand iid (only handSoothingMelody)
          whenM (getCanShuffleDeck iid) $ shuffleCardsIntoDeck iid deckSoothingMelodies
      HallowedMirror3 <$> liftRunMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      chooseOneM iid do
        labeled "Change each \"2\" to a \"3\""
          $ eventModifier (attrs.ability 1) card (MetaModifier $ object ["use3" .= True])
        labeled "Shuffle it into your deck instead of discarding it"
          $ eventModifier (attrs.ability 1) card (SetAfterPlay ShuffleThisBackIntoDeck)
      pure a
    _ -> HallowedMirror3 <$> liftRunMessage msg attrs
