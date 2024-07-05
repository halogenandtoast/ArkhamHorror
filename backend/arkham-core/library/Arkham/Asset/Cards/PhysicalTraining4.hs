module Arkham.Asset.Cards.PhysicalTraining4 (physicalTraining4, PhysicalTraining4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Modifier

newtype PhysicalTraining4 = PhysicalTraining4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining4 :: AssetCard PhysicalTraining4
physicalTraining4 = asset PhysicalTraining4 Cards.physicalTraining4

instance HasAbilities PhysicalTraining4 where
  getAbilities (PhysicalTraining4 a) =
    [ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ OrCost [ResourceCost 1, UseCost (be a) #resource 1]
    ]

instance RunMessage PhysicalTraining4 where
  runMessage msg a@(PhysicalTraining4 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . PhysicalTraining4 $ attrs & tokensL . ix #resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOne
        iid
        [ Label "Choose Willpower" [Msg.skillTestModifier (attrs.ability 1) iid (SkillModifier #willpower 1)]
        , Label "Choose Combat" [Msg.skillTestModifier (attrs.ability 1) iid (SkillModifier #combat 1)]
        ]
      pure a
    _ -> PhysicalTraining4 <$> liftRunMessage msg attrs
