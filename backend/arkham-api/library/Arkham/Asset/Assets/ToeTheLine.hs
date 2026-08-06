module Arkham.Asset.Assets.ToeTheLine (toeTheLine) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEnds)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Investigator.Types (Field (InvestigatorHealth))
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)

newtype ToeTheLine = ToeTheLine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toeTheLine :: AssetCard ToeTheLine
toeTheLine = asset ToeTheLine Cards.toeTheLine

instance HasAbilities ToeTheLine where
  getAbilities (ToeTheLine a) =
    [ controlled a 1 NoRestriction $ forced $ InvestigatorTakeDamage #when You AnySource
    , -- "if the amount of damage on this card equals or exceeds your maximum
      -- health" — maximum health is modifiable, so compare the two as a
      -- calculation instead of a fixed threshold.
      controlled
        a
        2
        ( HasCalculation
            ( SubtractCalculation
                (AssetTokenCountCalculation a.id #damage)
                (InvestigatorsFieldCalculation You InvestigatorHealth)
            )
            (atLeast 0)
        )
        $ forced taskEnds
    ]

instance RunMessage ToeTheLine where
  runMessage msg a@(ToeTheLine attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #damage 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.ToeTheLine 1
      pure a
    _ -> ToeTheLine <$> liftRunMessage msg attrs
