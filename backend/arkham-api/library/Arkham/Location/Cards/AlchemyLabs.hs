module Arkham.Location.Cards.AlchemyLabs (alchemyLabs, AlchemyLabs (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype AlchemyLabs = AlchemyLabs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemyLabs :: LocationCard AlchemyLabs
alchemyLabs = location AlchemyLabs Cards.alchemyLabs 5 (Static 0)

instance HasModifiersFor AlchemyLabs where
  getModifiersFor target (AlchemyLabs attrs) | isTarget attrs target = do
    toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities AlchemyLabs where
  getAbilities (AlchemyLabs attrs) =
    withRevealedAbilities
      attrs
      [ withTooltip
          ( "{action}: _Investigate_. If you are successful, instead of discovering clues, take the Alchemical Concoction from underneath this location if able"
              <> (if canTake then "" else " (YOU CANNOT)")
              <> "."
          )
          $ investigateAbility attrs 1 mempty Here
      ]
   where
    canTake = any (`cardMatch` cardIs Cards.alchemicalConcoction) (locationCardsUnderneath attrs)

instance RunMessage AlchemyLabs where
  runMessage msg l@(AlchemyLabs attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- genId
      pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      maid <- selectOne $ assetIs Cards.alchemicalConcoction
      for_ maid (push . TakeControlOfAsset iid)
      pure l
    _ -> AlchemyLabs <$> runMessage msg attrs
