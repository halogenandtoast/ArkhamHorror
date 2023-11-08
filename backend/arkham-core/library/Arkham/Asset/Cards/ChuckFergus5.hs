module Arkham.Asset.Cards.ChuckFergus5 (
  chuckFergus5,
  ChuckFergus5 (..),
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

newtype ChuckFergus5 = ChuckFergus5 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chuckFergus5 :: AssetCard ChuckFergus5
chuckFergus5 = ally ChuckFergus5 Cards.chuckFergus5 (2, 2)

cardMatcher :: CardMatcher
cardMatcher = CardWithOneOf [CardWithTrait Tactic, CardWithTrait Trick] <> CardWithType EventType

instance HasModifiersFor ChuckFergus5 where
  getModifiersFor (InvestigatorTarget iid) (ChuckFergus5 a)
    | controlledBy a iid && not (assetExhausted a) =
        pure
          $ toModifiers a [CanBecomeFast cardMatcher, CanReduceCostOf cardMatcher 2]
  getModifiersFor _ _ = pure []

instance HasAbilities ChuckFergus5 where
  getAbilities (ChuckFergus5 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (Matcher.PlayCard Timing.When You $ BasicCardMatch cardMatcher)
        $ ExhaustCost (toTarget a)
    ]

getWindowCard :: [Window] -> Card
getWindowCard [] = error "missing play card window"
getWindowCard ((windowType -> Window.PlayCard _ c) : _) = c
getWindowCard (_ : xs) = getWindowCard xs

instance RunMessage ChuckFergus5 where
  runMessage msg a@(ChuckFergus5 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws@(getWindowCard -> card@(PlayerCard pc)) _ -> do
      cost <- getModifiedCardCost iid card
      canAffordCost <-
        getCanAffordCost iid (PlayerCardSource pc) [Action.Play] ws (ResourceCost cost)
      canAffordActionCost <-
        getCanAffordCost iid (PlayerCardSource pc) [Action.Play] ws (ActionCost 1)
      -- can something else reduce the cost enough?
      let n = if canAffordCost then 2 else 1
          n' = if canAffordActionCost then n else n - 1
      player <- getPlayer iid
      when (n' > 0) $ do
        push
          $ chooseN player n'
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
            | canAffordActionCost
            ]
          <> [ Label
              "That event costs 2 fewer resources to play."
              [ CreateWindowModifierEffect
                  EffectCostWindow
                  ( EffectModifiers
                      $ toModifiers attrs [ReduceCostOf (CardWithId $ toCardId card) 2]
                  )
                  (toSource attrs)
                  (InvestigatorTarget iid)
              ]
             | canAffordCost
             ]
          <> [ Label
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
    _ -> ChuckFergus5 <$> runMessage msg attrs
