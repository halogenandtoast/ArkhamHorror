module Arkham.Asset.Assets.DreamsOfDestruction (dreamsOfDestruction) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEnds)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Investigator.Types (Field (InvestigatorSanity))
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)

newtype DreamsOfDestruction = DreamsOfDestruction AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfDestruction :: AssetCard DreamsOfDestruction
dreamsOfDestruction = asset DreamsOfDestruction Cards.dreamsOfDestruction

instance HasAbilities DreamsOfDestruction where
  getAbilities (DreamsOfDestruction a) =
    [ controlled a 1 NoRestriction $ forced $ InvestigatorTakeHorror #when You AnySource
    , -- "if the amount of horror on this card equals or exceeds your maximum
      -- sanity" — maximum sanity is modifiable, so compare the two as a
      -- calculation instead of a fixed threshold.
      controlled
        a
        2
        ( HasCalculation
            ( SubtractCalculation
                (AssetTokenCountCalculation a.id #horror)
                (InvestigatorsFieldCalculation You InvestigatorSanity)
            )
            (atLeast 0)
        )
        $ forced taskEnds
    ]

instance RunMessage DreamsOfDestruction where
  runMessage msg a@(DreamsOfDestruction attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #horror 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.DreamsOfDestruction 1
      pure a
    _ -> DreamsOfDestruction <$> liftRunMessage msg attrs
