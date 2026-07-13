module Arkham.Asset.Assets.InTheKnow1 (inTheKnow1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Cost
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype InTheKnow1 = InTheKnow1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheKnow1 :: AssetCard InTheKnow1
inTheKnow1 = asset InTheKnow1 Cards.inTheKnow1

instance HasAbilities InTheKnow1 where
  getAbilities (InTheKnow1 attrs) =
    [ withInvestigationTargets RevealedLocation
        $ restricted attrs 1 ControlsThis (investigateAction $ assetUseCost attrs Secret 1)
    ]

instance RunMessage InTheKnow1 where
  runMessage msg a@(InTheKnow1 attrs) = runQueueT $ case msg of
    UseCardAbility iid source 1 windows' _ | isSource attrs source -> do
      locations <- select $ RevealedLocation <> InvestigatableLocation
      locationsWithInvestigate <- concatForM locations \lid -> do
        investigateActions <- select $ AbilityOnLocation (be lid) <> #investigate
        costs <- getAdditionalActionCost iid (toTarget lid) #investigate
        canAfford <- getCanAffordAdditionalActionCost iid attrs (toTarget lid) #investigate
        pure $ if canAfford then map (lid,costs,) investigateActions else []
      batchId <- getRandom
      chooseOneM iid do
        for_ locationsWithInvestigate \(location, costs, ability) -> do
          targeting location do
            batching batchId do
              abilityModifier ability.ref (attrs.ability 1) iid (AsIfAt location)
              push $ PayAdditionalCost iid batchId costs
              push $ UseAbility iid (decreaseAbilityActionCost ability 1) windows'
      pure a
    _ -> InTheKnow1 <$> liftRunMessage msg attrs
