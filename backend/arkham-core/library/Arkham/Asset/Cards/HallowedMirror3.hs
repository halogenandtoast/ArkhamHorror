module Arkham.Asset.Cards.HallowedMirror3 (hallowedMirror3, HallowedMirror3) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (getCanShuffleDeck, searchBonded)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype HallowedMirror3 = HallowedMirror3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

hallowedMirror3 :: AssetCard HallowedMirror3
hallowedMirror3 = asset HallowedMirror3 Cards.hallowedMirror3

instance HasAbilities HallowedMirror3 where
  getAbilities (HallowedMirror3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ Matcher.PlayCard #when You
        $ BasicCardMatch
        $ cardIs Events.soothingMelody
    ]

getCard :: [Window] -> Card
getCard = \case
  ((windowType -> Window.PlayCard _ card) : _) -> card
  _ -> error "impossible"

instance RunMessage HallowedMirror3 where
  runMessage msg a@(HallowedMirror3 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      bonded <- take 3 <$> searchBonded iid Events.soothingMelody
      case bonded of
        [] -> pure ()
        (handSoothingMelody : deckSoothingMelodies) -> do
          canShuffleDeck <- getCanShuffleDeck iid
          pushAll
            $ addToHand iid handSoothingMelody
            : [ShuffleCardsIntoDeck (InvestigatorDeck iid) deckSoothingMelodies | canShuffleDeck]
      HallowedMirror3 <$> runMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
              "Change each \"2\" to a \"3\""
              [eventModifier (toAbilitySource attrs 1) (toCardId card) (MetaModifier $ object ["use3" .= True])]
          , Label
              "Shuffle it into your deck instead of discarding it"
              [eventModifier (toAbilitySource attrs 1) (toCardId card) (SetAfterPlay ShuffleThisBackIntoDeck)]
          ]
      pure a
    _ -> HallowedMirror3 <$> runMessage msg attrs
