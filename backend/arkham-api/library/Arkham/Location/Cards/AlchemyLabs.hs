module Arkham.Location.Cards.AlchemyLabs (alchemyLabs, AlchemyLabs (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AlchemyLabs = AlchemyLabs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemyLabs :: LocationCard AlchemyLabs
alchemyLabs = location AlchemyLabs Cards.alchemyLabs 5 (Static 0)

instance HasModifiersFor AlchemyLabs where
  getModifiersFor (AlchemyLabs a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities AlchemyLabs where
  getAbilities (AlchemyLabs attrs) =
    extendRevealed1 attrs
      $ withTooltip
        ( "{action}: _Investigate_. If you are successful, instead of discovering clues, take the Alchemical Concoction from underneath this location if able"
            <> (if canTake then "" else " (YOU CANNOT)")
            <> "."
        )
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
