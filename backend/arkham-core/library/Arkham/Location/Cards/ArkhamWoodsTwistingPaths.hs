module Arkham.Location.Cards.ArkhamWoodsTwistingPaths where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsTwistingPaths)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: LocationCard ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths = location ArkhamWoodsTwistingPaths Cards.arkhamWoodsTwistingPaths 3 (PerPlayer 1)

instance HasAbilities ArkhamWoodsTwistingPaths where
  getAbilities (ArkhamWoodsTwistingPaths attrs) =
    withRevealedAbilities attrs
      $ [ forcedAbility attrs 1
            $ Leaves Timing.When You
            $ LocationWithId (toId attrs)
        ]

-- TODO: Batch cancel
instance RunMessage ArkhamWoodsTwistingPaths where
  runMessage msg l@(ArkhamWoodsTwistingPaths attrs) = case msg of
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
      pushAll
        [ CreateEffect "01151" effectMetadata (toAbilitySource attrs 1) target
        , beginSkillTest iid (toAbilitySource attrs 1) target #intellect 3
        ]
      pure l
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
