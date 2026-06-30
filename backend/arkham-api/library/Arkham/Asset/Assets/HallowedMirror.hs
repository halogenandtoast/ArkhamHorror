module Arkham.Asset.Assets.HallowedMirror where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (searchBonded)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype HallowedMirror = HallowedMirror AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallowedMirror :: AssetCard HallowedMirror
hallowedMirror = asset HallowedMirror Cards.hallowedMirror

instance HasAbilities HallowedMirror where
  getAbilities (HallowedMirror a) =
    [controlled_ a 1 $ forced $ Matcher.AssetEntersPlay #after (be a)]

instance RunMessage HallowedMirror where
  runMessage msg a@(HallowedMirror attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      bonded <- take 3 <$> searchBonded iid Events.soothingMelody
      for_ (nonEmpty bonded) \(handSoothingMelody :| deckSoothingMelodies) -> do
        addToHand iid (only handSoothingMelody)
        shuffleCardsIntoDeck iid deckSoothingMelodies
      pure a
    RemovedFromPlay (isSource attrs -> True) -> do
      -- v2.0 errata (faq/card_errata.md): "set them aside, out of play" instead
      -- of "remove them from the game" — matches Occult Lexicon / Miss Doyle.
      for_ attrs.owner \iid -> do
        soothingMelodies <- select $ basic $ CardOwnedBy iid <> cardIs Events.soothingMelody
        for_ soothingMelodies $ placeInBonded iid
      HallowedMirror <$> liftRunMessage msg attrs
    _ -> HallowedMirror <$> liftRunMessage msg attrs
