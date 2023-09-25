module Arkham.Asset.Cards.AnnaKaslow4 (
  annaKaslow4,
  AnnaKaslow4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Tarot))
import Arkham.Window (defaultWindows)

newtype AnnaKaslow4 = AnnaKaslow4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

annaKaslow4 :: AssetCard AnnaKaslow4
annaKaslow4 = ally AnnaKaslow4 Cards.annaKaslow4 (1, 1)

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) Nothing

instance HasAbilities AnnaKaslow4 where
  getAbilities (AnnaKaslow4 a) =
    [ restrictedAbility a 1 InYourHand
        $ ReactionAbility (GameBegins Timing.When) Free
    , restrictedAbility
        a
        2
        (ControlsThis <> CanSearchDeck <> CanShuffleDeck)
        ( ReactionAbility (AssetEntersPlay Timing.When $ AssetWithId (toId a)) Free
        )
    ]

instance RunMessage AnnaKaslow4 where
  runMessage msg a@(AnnaKaslow4 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid TarotSlot (slot attrs))
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push
        $ search iid attrs iid [fromDeck] (CardWithTrait Tarot <> CardWithType AssetType)
        $ PlayFound iid 1
      pure a
    _ -> AnnaKaslow4 <$> runMessage msg attrs
