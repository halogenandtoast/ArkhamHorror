module Arkham.Asset.Cards.TheHierophantV3 (
  theHierophantV3,
  TheHierophantV3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype TheHierophantV3 = TheHierophantV3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHierophantV3 :: AssetCard TheHierophantV3
theHierophantV3 = asset TheHierophantV3 Cards.theHierophantV3

instance HasAbilities TheHierophantV3 where
  getAbilities (TheHierophantV3 a) =
    [ restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)
    ]

instance HasModifiersFor TheHierophantV3 where
  getModifiersFor (InvestigatorTarget iid) (TheHierophantV3 attrs) | attrs `controlledBy` iid = do
    pure
      $ toModifiers
        attrs
        [ SlotCanBe ArcaneSlot AccessorySlot
        , SlotCanBe AccessorySlot ArcaneSlot
        ]
  getModifiersFor _ _ = pure []

instance RunMessage TheHierophantV3 where
  runMessage msg a@(TheHierophantV3 attrs) = case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid ArcaneSlot (Slot (toSource attrs) [])
      TheHierophantV3 <$> runMessage msg attrs
    _ -> TheHierophantV3 <$> runMessage msg attrs
