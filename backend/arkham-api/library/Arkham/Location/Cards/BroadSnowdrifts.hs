module Arkham.Location.Cards.BroadSnowdrifts (broadSnowdrifts, BroadSnowdrifts (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.SkillTest (discoverAdditionalClues)

newtype BroadSnowdrifts = BroadSnowdrifts LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

broadSnowdrifts :: LocationCard BroadSnowdrifts
broadSnowdrifts =
  symbolLabel
    $ locationWith BroadSnowdrifts Cards.broadSnowdrifts 1 (PerPlayer 2)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities BroadSnowdrifts where
  getAbilities (BroadSnowdrifts a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ ReactionAbility (SkillTestResult #when You (whileInvestigating a) #success) (AddFrostTokenCost 3)

instance RunMessage BroadSnowdrifts where
  runMessage msg l@(BroadSnowdrifts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAdditionalClues (attrs.ability 1) iid 3
      pure l
    _ -> BroadSnowdrifts <$> liftRunMessage msg attrs
