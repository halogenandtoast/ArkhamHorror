module Arkham.Location.Cards.SubmergedTemple (submergedTemple, SubmergedTemple (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (AncientOne))

newtype SubmergedTemple = SubmergedTemple LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

submergedTemple :: LocationCard SubmergedTemple
submergedTemple =
  locationWith SubmergedTemple Cards.submergedTemple 0 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ FullyFlooded)

instance HasModifiersFor SubmergedTemple where
  getModifiersFor (SubmergedTemple a) =
    modifySelect a (at_ (be a) <> EnemyWithTrait AncientOne) [EnemyFight 2]

instance HasAbilities SubmergedTemple where
  getAbilities (SubmergedTemple a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost
      $ PlaceKeyCost (toTarget a) GreenKey
      <> GroupClueCost (PerPlayer 1) (be a)

instance RunMessage SubmergedTemple where
  runMessage msg l@(SubmergedTemple attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (EnemyWithTrait AncientOne) $ automaticallyEvadeEnemy iid
      setThisFloodLevel attrs Unflooded
      gameModifier (attrs.ability 1) attrs CannotBeFlooded
      pure l
    _ -> SubmergedTemple <$> liftRunMessage msg attrs
