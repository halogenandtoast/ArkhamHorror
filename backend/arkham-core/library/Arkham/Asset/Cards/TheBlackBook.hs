module Arkham.Asset.Cards.TheBlackBook (
  theBlackBook,
  TheBlackBook (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheBlackBook = TheBlackBook AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackBook :: AssetCard TheBlackBook
theBlackBook = asset TheBlackBook Cards.theBlackBook

instance HasModifiersFor TheBlackBook where
  getModifiersFor (InvestigatorTarget iid) (TheBlackBook a)
    | controlledBy a iid = do
        sanity <- field InvestigatorRemainingSanity iid
        pure $
          toModifiers
            a
            [ SkillModifier SkillWillpower 1
            , SkillModifier SkillIntellect 1
            , CanReduceCostOf AnyCard sanity
            ]
  getModifiersFor _ _ = pure []

instance HasAbilities TheBlackBook where
  getAbilities (TheBlackBook a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch AnyCard))
          (ExhaustCost (toTarget a) <> HorrorCostX (toSource a))
    ]

toHorror :: Payment -> Int
toHorror = \case
  HorrorPayment n -> n
  Payments ps -> sum $ map toHorror ps
  _ -> 0

windowToCard :: [Window] -> Card
windowToCard [] = error "invalid"
windowToCard (Window _ (Window.PlayCard _ card) : _) = card
windowToCard (_ : xs) = windowToCard xs

instance RunMessage TheBlackBook where
  runMessage msg a@(TheBlackBook attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (windowToCard -> card) (toHorror -> n) -> do
      push $
        createCostModifiers
          attrs
          card
          [ReduceCostOf (CardWithId $ toCardId card) n]
      pure a
    _ -> TheBlackBook <$> runMessage msg attrs
