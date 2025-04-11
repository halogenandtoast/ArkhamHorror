module Arkham.Location.Cards.AlchemyLabs (alchemyLabs) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ExtracurricularActivity.Helpers

newtype AlchemyLabs = AlchemyLabs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemyLabs :: LocationCard AlchemyLabs
alchemyLabs = symbolLabel $ location AlchemyLabs Cards.alchemyLabs 5 (Static 0)

instance HasModifiersFor AlchemyLabs where
  getModifiersFor (AlchemyLabs a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities AlchemyLabs where
  getAbilities (AlchemyLabs attrs) =
    extendRevealed1 attrs
      $ scenarioI18n
      $ withI18nTooltip ("alchemyLabs.action." <> if canTake then "can" else "cannot")
      $ investigateAbility attrs 1 mempty Here
   where
    canTake = any (`cardMatch` cardIs Cards.alchemicalConcoction) (locationCardsUnderneath attrs)

instance RunMessage AlchemyLabs where
  runMessage msg l@(AlchemyLabs attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      maid <- selectOne $ assetIs Cards.alchemicalConcoction
      for_ maid $ takeControlOfAsset iid
      pure l
    _ -> AlchemyLabs <$> liftRunMessage msg attrs
