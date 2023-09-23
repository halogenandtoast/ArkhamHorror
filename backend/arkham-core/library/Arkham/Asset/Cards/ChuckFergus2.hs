module Arkham.Asset.Cards.ChuckFergus2 (
  chuckFergus2,
  ChuckFergus2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Tactic, Trick))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ChuckFergus2 = ChuckFergus2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chuckFergus2 :: AssetCard ChuckFergus2
chuckFergus2 = ally ChuckFergus2 Cards.chuckFergus2 (2, 2)

cardMatcher :: CardMatcher
cardMatcher = CardWithOneOf [CardWithTrait Tactic, CardWithTrait Trick] <> CardWithType EventType

-- If another card uses this we will need to figure out how to keep track of
-- which thing has applied which modifiers. I think we are safe with the
-- current card pool
instance HasModifiersFor ChuckFergus2 where
  getModifiersFor (InvestigatorTarget iid) (ChuckFergus2 a)
    | controlledBy a iid && not (assetExhausted a) =
        pure
          $ toModifiers a [CanBecomeFastOrReduceCostOf cardMatcher 2]
  getModifiersFor _ _ = pure []

instance HasAbilities ChuckFergus2 where
  getAbilities (ChuckFergus2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (Matcher.PlayCard Timing.When You $ BasicCardMatch cardMatcher)
        $ ExhaustCost (toTarget a)
    ]

getWindowCard :: [Window] -> Card
getWindowCard [] = error "missing play card window"
getWindowCard ((windowType -> Window.PlayCard _ c) : _) = c
getWindowCard (_ : xs) = getWindowCard xs

instance RunMessage ChuckFergus2 where
  runMessage msg a@(ChuckFergus2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws@(getWindowCard -> card@(PlayerCard pc)) _ -> do
      cost <- getModifiedCardCost iid card
      canAffordCost <-
        getCanAffordCost iid (PlayerCardSource pc) (Just Action.Play) ws (ResourceCost cost)
      canAffordActionCost <-
        getCanAffordCost iid (PlayerCardSource pc) (Just Action.Play) ws (ActionCost 1)
      -- can something else reduce the cost enough?
      when (canAffordCost && canAffordActionCost) $ do
        push
          $ chooseOne iid
          $ [ Label
                "That event gains fast"
                [ CreateWindowModifierEffect
                    EffectEventWindow
                    ( EffectModifiers
                        $ toModifiers attrs [BecomesFast]
                    )
                    (toSource attrs)
                    (CardIdTarget $ toCardId card)
                ]
            , Label
                "That event costs 2 fewer resources to play."
                [ CreateWindowModifierEffect
                    EffectCostWindow
                    ( EffectModifiers
                        $ toModifiers attrs [ReduceCostOf (CardWithId $ toCardId card) 2]
                    )
                    (toSource attrs)
                    (InvestigatorTarget iid)
                ]
            , Label
                "You get +2 skill value while performing a skill test during the resolution of that event."
                [ CreateWindowModifierEffect
                    EffectEventWindow
                    ( EffectModifiers
                        $ toModifiers attrs [AnySkillValue 2]
                    )
                    (toSource attrs)
                    (InvestigatorTarget iid)
                ]
            ]

      unless canAffordActionCost
        $ push
        $ CreateWindowModifierEffect
          EffectEventWindow
          ( EffectModifiers
              $ toModifiers attrs [BecomesFast]
          )
          (toSource attrs)
          (CardIdTarget $ toCardId card)

      unless canAffordCost
        $ push
        $ CreateWindowModifierEffect
          EffectCostWindow
          ( EffectModifiers
              $ toModifiers attrs [ReduceCostOf (CardWithId $ toCardId card) 2]
          )
          (toSource attrs)
          (InvestigatorTarget iid)

      pure a
    _ -> ChuckFergus2 <$> runMessage msg attrs
