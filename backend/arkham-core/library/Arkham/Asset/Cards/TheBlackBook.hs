module Arkham.Asset.Cards.TheBlackBook (
  theBlackBook,
  TheBlackBook (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing

-- Reaction: When you play a card, exhaust The Black Book and take X horror: Reduce that card's cost by X.

newtype TheBlackBook = TheBlackBook AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackBook :: AssetCard TheBlackBook
theBlackBook = asset TheBlackBook Cards.theBlackBook

instance HasModifiersFor TheBlackBook where
  getModifiersFor (InvestigatorTarget iid) (TheBlackBook a)
    | controlledBy a iid = do
        horror <- field InvestigatorHorror iid
        pure $
          toModifiers
            a
            [SkillModifier SkillWillpower 1, SkillModifier SkillIntellect 1, CanReduceCostOf AnyCard horror]
  getModifiersFor _ _ = pure []

instance HasAbilities TheBlackBook where
  getAbilities (TheBlackBook a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          (PlayCard Timing.After You (BasicCardMatch AnyCard))
          (ExhaustCost $ toTarget a)
    ]

toHorror :: Payment -> Int
toHorror = \case
  HorrorPayment n -> n
  Payments ps -> sum $ map toHorror ps
  _ -> 0

instance RunMessage TheBlackBook where
  runMessage msg a@(TheBlackBook attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (toHorror -> _n) -> do
      pure a
    _ -> TheBlackBook <$> runMessage msg attrs
