module Arkham.Asset.Assets.InTheKnow1 (inTheKnow1, InTheKnow1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype InTheKnow1 = InTheKnow1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheKnow1 :: AssetCard InTheKnow1
inTheKnow1 = asset InTheKnow1 Cards.inTheKnow1

instance HasAbilities InTheKnow1 where
  getAbilities (InTheKnow1 attrs) =
    [ delayAdditionalCostsWhen (exists $ RevealedLocation <> InvestigatableLocation)
        $ restricted attrs 1 ControlsThis (investigateAction $ assetUseCost attrs Secret 1)
        & (abilityMetadataL ?~ InvestigateTargets RevealedLocation)
    ]

instance RunMessage InTheKnow1 where
  runMessage msg a@(InTheKnow1 attrs) = runQueueT $ case msg of
    UseCardAbility iid source 1 windows' _ | isSource attrs source -> do
      locations <- select $ RevealedLocation <> InvestigatableLocation
      locationsWithInvestigate <- concatForM locations \lid -> do
        investigateActions <- select $ AbilityOnLocation (be lid) <> #investigate
        mods <- getModifiers lid
        let costs = fold [m | AdditionalCostToInvestigate m <- mods]
        pure $ map (lid,costs,) investigateActions
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
