module Arkham.Location.Cards.ArkhamWoodsTwistingPaths where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsTwistingPaths )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: LocationCard ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths = location
  ArkhamWoodsTwistingPaths
  Cards.arkhamWoodsTwistingPaths
  3
  (PerPlayer 1)

instance HasAbilities ArkhamWoodsTwistingPaths where
  getAbilities (ArkhamWoodsTwistingPaths attrs) | locationRevealed attrs =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ Leaves Timing.When You
          $ LocationWithId
          $ toId attrs
        ]
  getAbilities (ArkhamWoodsTwistingPaths attrs) = getAbilities attrs

instance RunMessage ArkhamWoodsTwistingPaths where
  runMessage msg l@(ArkhamWoodsTwistingPaths attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      moveFrom <- popMessageMatching \case
        MoveFrom _ iid' lid' -> iid' == iid && toId l == lid'
        _ -> False
      moveTo <- popMessageMatching \case
        MoveTo _ iid' _ -> iid == iid' -- we don't know where they are going for the cancel
        _ -> False
      let
        target = InvestigatorTarget iid
        effectMetadata = Just $ EffectMessages (catMaybes [moveFrom, moveTo])
      l <$ pushAll
        [ CreateEffect "01151" effectMetadata source target
        , BeginSkillTest iid source target Nothing SkillIntellect 3
        ]
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
