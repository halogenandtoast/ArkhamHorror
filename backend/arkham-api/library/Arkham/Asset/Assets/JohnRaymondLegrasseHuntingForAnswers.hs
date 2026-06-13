module Arkham.Asset.Assets.JohnRaymondLegrasseHuntingForAnswers (johnRaymondLegrasse) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype JohnRaymondLegrasseHuntingForAnswers = JohnRaymondLegrasseHuntingForAnswers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
johnRaymondLegrasse :: AssetCard JohnRaymondLegrasseHuntingForAnswers
johnRaymondLegrasse = ally JohnRaymondLegrasseHuntingForAnswers Cards.johnRaymondLegrasse (3, 3)

instance RunMessage JohnRaymondLegrasseHuntingForAnswers where
  runMessage msg (JohnRaymondLegrasseHuntingForAnswers attrs) =
    runQueueT $ JohnRaymondLegrasseHuntingForAnswers <$> liftRunMessage msg attrs
