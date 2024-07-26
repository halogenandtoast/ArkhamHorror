module Arkham.Location.Cards.NarrowShaft (narrowShaft, narrowShaftEffect, NarrowShaft (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.Effect.Runner hiding (RevealLocation)
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.SkillType

newtype NarrowShaft = NarrowShaft LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowShaft :: LocationCard NarrowShaft
narrowShaft =
  locationWith NarrowShaft Cards.narrowShaft 2 (PerPlayer 1)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation])

instance HasAbilities NarrowShaft where
  getAbilities (NarrowShaft attrs) =
    withRevealedAbilities
      attrs
      [ skillTestAbility
          $ mkAbility attrs 1
          $ forced
          $ Moves #when You AnySource (be attrs) UnrevealedLocation
      , restrictedAbility
          attrs
          2
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [Above, Below, RightOf]])
          $ forced (RevealLocation #when Anyone $ be attrs)
      ]

instance RunMessage NarrowShaft where
  runMessage msg l@(NarrowShaft attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      moveFrom <- popMessageMatching \case
        MoveFrom _ iid' lid' -> iid' == iid && toId l == lid'
        _ -> False
      moveTo <- popMessageMatching \case
        MoveTo movement -> moveTarget movement == InvestigatorTarget iid -- we don't know where they are going for the cancel
        _ -> False
      let
        target = InvestigatorTarget iid
        effectMetadata = Just $ EffectMessages (catMaybes [moveFrom, moveTo])
      sid <- getRandom
      pushAll
        [ createCardEffect Cards.narrowShaft effectMetadata (attrs.ability 1) target
        , beginSkillTest sid iid (attrs.ability 1) target SkillAgility (Fixed 3)
        ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [card] -> do
          placeAbove <- placeAtDirection Above attrs >>= \f -> f card
          placeRight <- placeAtDirection RightOf attrs >>= \f -> f card
          aboveEmpty <- directionEmpty attrs Above
          rightEmpty <- directionEmpty attrs RightOf
          player <- getPlayer iid
          push
            $ chooseOrRunOne player
            $ [Label "Place Above" placeAbove | aboveEmpty]
            <> [Label "Place to the Right" placeRight | rightEmpty]
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> NarrowShaft <$> runMessage msg attrs

newtype NarrowShaftEffect = NarrowShaftEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowShaftEffect :: EffectArgs -> NarrowShaftEffect
narrowShaftEffect = cardEffect NarrowShaftEffect Cards.narrowShaft

instance RunMessage NarrowShaftEffect where
  runMessage msg e@(NarrowShaftEffect attrs) = case msg of
    PassedThisSkillTest _ (AbilitySource (LocationSource lid) 1) -> do
      narrowShaftEffectId <- getJustLocationByName "Narrow Shaft"
      when (lid == narrowShaftEffectId)
        $ case effectMetadata attrs of
          Just (EffectMessages msgs) -> pushAll (msgs <> [disable attrs])
          _ -> push $ disable attrs
      pure e
    FailedThisSkillTest iid (AbilitySource (LocationSource lid) 1) -> do
      narrowShaftEffectId <- getJustLocationByName "Narrow Shaft"
      when (lid == narrowShaftEffectId)
        $ pushAll [assignDamage iid narrowShaftEffectId 1, disable attrs]
      pure e
    _ -> NarrowShaftEffect <$> runMessage msg attrs
