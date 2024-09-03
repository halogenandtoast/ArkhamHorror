module Arkham.Asset.Cards.EmbezzeledTreasure (embezzeledTreasure, EmbezzeledTreasure (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token

newtype EmbezzeledTreasure = EmbezzeledTreasure AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

embezzeledTreasure :: AssetCard EmbezzeledTreasure
embezzeledTreasure = asset EmbezzeledTreasure Cards.embezzeledTreasure

instance HasAbilities EmbezzeledTreasure where
  getAbilities (EmbezzeledTreasure a) =
    [ restrictedAbility a 1 (ControlsThis <> youExist InvestigatorWithAnyResources <> resourceRestriction)
        $ FastAbility (exhaust a)
    , restrictedAbility a 2 (ControlsThis <> requiredResources)
        $ forced
        $ oneOf [GameEnds #when, InvestigatorResigned #when You]
    ]
   where
    resourceRestriction = if a.use Resource >= 10 then Never else NoRestriction
    requiredResources = if a.use Resource >= 2 then NoRestriction else Never

instance RunMessage EmbezzeledTreasure where
  runMessage msg a@(EmbezzeledTreasure attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- field InvestigatorResources iid
      if resources > 1 || attrs.use Resource == 9
        then chooseAmount iid "Resources" "Resources" 1 2 attrs
        else push $ MoveTokens (attrs.ability 1) (ResourceSource iid) (toTarget attrs) Resource 1
      pure a
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) -> do
      push $ MoveTokens (attrs.ability 1) (ResourceSource iid) (toTarget attrs) Resource n
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigators <- select $ affectsOthers Anyone
      chooseOrRunOne iid $ targetLabels investigators $ only . handleTargetChoice iid (attrs.ability 2)
      pure a
    HandleTargetChoice _ (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid) -> do
      let n = attrs.use Resource `div` 2
      setupModifier (attrs.ability 2) iid (StartingResources n)
      pure a
    _ -> EmbezzeledTreasure <$> liftRunMessage msg attrs
