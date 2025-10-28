module Arkham.Location.Cards.SpaceSet (spaceSet) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SpaceSet = SpaceSet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spaceSet :: LocationCard SpaceSet
spaceSet = location SpaceSet Cards.spaceSet 3 (PerPlayer 1)

instance HasAbilities SpaceSet where
  getAbilities (SpaceSet a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)
      , playerLimit PerGame $ restricted a 2 (Here <> playableCard) actionAbility
      ]
   where
    playableCard = PlayableCardExistsWithCostReduction (Reduce 2) (InHandOf ForPlay You <> #asset)

instance RunMessage SpaceSet where
  runMessage msg l@(SpaceSet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      cards <- select (inHandOf ForPlay iid <> #asset)
      chooseTargetM iid cards \c -> do
        reduceCostOf (attrs.ability 2) c 2
        playCardPayingCost iid c

      pure l
    _ -> SpaceSet <$> liftRunMessage msg attrs
