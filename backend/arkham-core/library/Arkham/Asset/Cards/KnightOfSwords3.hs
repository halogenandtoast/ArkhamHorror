module Arkham.Asset.Cards.KnightOfSwords3 (
  knightOfSwords3,
  KnightOfSwords3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype KnightOfSwords3 = KnightOfSwords3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

knightOfSwords3 :: AssetCard KnightOfSwords3
knightOfSwords3 =
  asset KnightOfSwords3 Cards.knightOfSwords3

instance HasAbilities KnightOfSwords3 where
  getAbilities (KnightOfSwords3 a) =
    [ reactionAbility
        a
        1
        Free
        (WouldHaveSkillTestResult #when You AnySkillTest $ SuccessResult AnyValue)
        ControlsThis
    , restrictedAbility a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

instance RunMessage KnightOfSwords3 where
  runMessage msg a@(KnightOfSwords3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
              "Discard Knight of Swords to get +3 instead"
              [skillTestModifier attrs iid (AnySkillValue 3), RecalculateSkillTestResults]
          , Label "Do not discard" [skillTestModifier attrs iid (AnySkillValue 1), RecalculateSkillTestResults]
          ]
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    _ -> KnightOfSwords3 <$> runMessage msg attrs
