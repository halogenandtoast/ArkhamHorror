module Arkham.Asset.Assets.LaboratoryAssistant (LaboratoryAssistant (..), laboratoryAssistant) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype LaboratoryAssistant = LaboratoryAssistant AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laboratoryAssistant :: AssetCard LaboratoryAssistant
laboratoryAssistant = ally LaboratoryAssistant Cards.laboratoryAssistant (1, 2)

instance HasModifiersFor LaboratoryAssistant where
  getModifiersFor (LaboratoryAssistant attrs) = controllerGets attrs [HandSize 2]

instance HasAbilities LaboratoryAssistant where
  getAbilities (LaboratoryAssistant x) =
    [ restrictedAbility x 1 ControlsThis
        $ freeReaction
        $ AssetEntersPlay #when (AssetWithId $ toId x)
    ]

instance RunMessage LaboratoryAssistant where
  runMessage msg a@(LaboratoryAssistant attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (toAbilitySource attrs 1) 2
      pure a
    _ -> LaboratoryAssistant <$> runMessage msg attrs
