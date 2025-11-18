module Arkham.Asset.Assets.HallowedMirror where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Deck
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (getCanShuffleDeck, searchBonded)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

newtype HallowedMirror = HallowedMirror AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallowedMirror :: AssetCard HallowedMirror
hallowedMirror = asset HallowedMirror Cards.hallowedMirror

instance HasAbilities HallowedMirror where
  getAbilities (HallowedMirror a) =
    [ controlled_ a 2 $ forced $ Matcher.AssetEntersPlay #after (be a)
    ]

instance RunMessage HallowedMirror where
  runMessage msg a@(HallowedMirror attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      bonded <- take 3 <$> searchBonded iid Events.soothingMelody
      case bonded of
        [] -> pure ()
        (handSoothingMelody : deckSoothingMelodies) -> do
          canShuffleDeck <- getCanShuffleDeck iid
          pushAll
            $ addToHand iid handSoothingMelody
            : [ShuffleCardsIntoDeck (InvestigatorDeck iid) deckSoothingMelodies | canShuffleDeck]
      pure a
    RemovedFromPlay (isSource attrs -> True) -> do
      let iid = getOwner attrs
      push (RemoveAllCopiesOfCardFromGame iid "05314")
      HallowedMirror <$> runMessage msg attrs
    _ -> HallowedMirror <$> runMessage msg attrs
