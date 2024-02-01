module Arkham.Asset.Cards.HiredMuscle1 (
  hiredMuscle1,
  HiredMuscle1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype HiredMuscle1 = HiredMuscle1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hiredMuscle1 :: AssetCard HiredMuscle1
hiredMuscle1 = ally HiredMuscle1 Cards.hiredMuscle1 (3, 1)

instance HasAbilities HiredMuscle1 where
  getAbilities (HiredMuscle1 x) = [restrictedAbility x 1 ControlsThis $ ForcedAbility $ PhaseEnds #when #upkeep]

instance HasModifiersFor HiredMuscle1 where
  getModifiersFor (InvestigatorTarget iid) (HiredMuscle1 a) =
    pure $ toModifiers a [SkillModifier #combat 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage HiredMuscle1 where
  runMessage msg a@(HiredMuscle1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Pay 1 Resource to Hired Muscle" [SpendResources iid 1]
          , Label "Discard Hired Muscle" [toDiscardBy iid (toAbilitySource attrs 1) attrs]
          ]
      pure a
    _ -> HiredMuscle1 <$> runMessage msg attrs
