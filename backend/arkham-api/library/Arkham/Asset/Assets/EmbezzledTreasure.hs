module Arkham.Asset.Assets.EmbezzledTreasure (embezzledTreasure) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Investigator.Types (Field (InvestigatorName, InvestigatorResources))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name (toTitle)
import Arkham.Projection
import Arkham.Token

newtype EmbezzledTreasure = EmbezzledTreasure AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

embezzledTreasure :: AssetCard EmbezzledTreasure
embezzledTreasure = asset EmbezzledTreasure Cards.embezzledTreasure

instance HasAbilities EmbezzledTreasure where
  getAbilities (EmbezzledTreasure a) =
    [ controlled a 1 (youExist InvestigatorWithAnyResources <> resourceRestriction)
        $ FastAbility (exhaust a)
    , controlled a 2 requiredResources
        $ forced
        $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
    ]
   where
    resourceRestriction = if a.use #resource >= 10 then Never else NoRestriction
    requiredResources = if a.use #resource >= 2 then NoRestriction else Never

instance RunMessage EmbezzledTreasure where
  runMessage msg a@(EmbezzledTreasure attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- field InvestigatorResources iid
      if resources > 1 || attrs.use Resource == 9
        then chooseAmount iid "Resources" "Resources" 1 2 attrs
        else moveTokens (attrs.ability 1) (ResourceSource iid) attrs #resource 1
      pure a
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) -> do
      moveTokens (attrs.ability 1) (ResourceSource iid) attrs #resource n
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let total = attrs.use Resource `div` 2
      investigators <- select $ affectsOthers Anyone
      named <- forToSnd investigators \i -> toTitle <$> field InvestigatorName i
      chooseAmounts
        iid
        "Distribute starting resources"
        (TotalAmountTarget total)
        [(name, (0, total)) | (_, name) <- named]
        (ProxyTarget (toTarget attrs) (toTarget attrs))
      pure a
    ResolveAmounts _ choices (ProxyTarget (isTarget attrs -> True) _) -> do
      investigators <- select $ affectsOthers Anyone
      named <- forToSnd investigators \i -> toTitle <$> field InvestigatorName i
      for_ named \(iid', name) -> do
        let n = getChoiceAmount name choices
        when (n > 0) $ setupModifier (attrs.ability 2) iid' (StartingResources n)
      pure a
    _ -> EmbezzledTreasure <$> liftRunMessage msg attrs
