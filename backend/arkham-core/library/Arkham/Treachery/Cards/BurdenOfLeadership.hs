module Arkham.Treachery.Cards.BurdenOfLeadership (burdenOfLeadership, BurdenOfLeadership (..)) where

import Arkham.Classes.HasQueue
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BurdenOfLeadership = BurdenOfLeadership TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burdenOfLeadership :: TreacheryCard BurdenOfLeadership
burdenOfLeadership = treachery BurdenOfLeadership Cards.burdenOfLeadership

instance RunMessage BurdenOfLeadership where
  runMessage msg t@(BurdenOfLeadership attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allies <- select $ assetControlledBy iid <> #ally
      choices <- forToSnd allies \ally -> evalQueueT $ do
        isReady <- ally <=~> AssetReady
        chooseOne iid
          $ [Label "Exhaust" [Exhaust (toTarget ally)] | isReady]
          <> [ Label
                "Deal 1 direct damage and 1 direct horror"
                [AssetDamageWithCheck ally (toSource attrs) 1 1 True]
             ]
      chooseOneAtATime iid $ map (uncurry targetLabel) choices
      pure t
    _ -> BurdenOfLeadership <$> liftRunMessage msg attrs
