module Arkham.Asset.Assets.HallowedMirror3 (hallowedMirror3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (searchBonded)
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
    [ controlled_ a 1
        $ freeReaction
        $ Matcher.PlayCard #when You (basic $ cardIs Events.soothingMelody)
    , controlled_ a 2 $ forced $ Matcher.AssetEntersPlay #after (be a)
    ]

instance RunMessage HallowedMirror3 where
  runMessage msg a@(HallowedMirror3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      chooseOneM iid do
        labeled "Change each \"2\" to a \"3\""
          $ eventModifier (attrs.ability 1) card (MetaModifier $ object ["use3" .= True])
        labeled "Shuffle it into your deck instead of discarding it"
          $ eventModifier (attrs.ability 1) card (SetAfterPlay ShuffleThisBackIntoDeck)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      bonded <- take 3 <$> searchBonded iid Events.soothingMelody
      for_ (nonEmpty bonded) \(handSoothingMelody :| deckSoothingMelodies) -> do
        addToHand iid (only handSoothingMelody)
        shuffleCardsIntoDeck iid deckSoothingMelodies
      pure a
    _ -> HallowedMirror3 <$> liftRunMessage msg attrs
