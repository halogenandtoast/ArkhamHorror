module Arkham.Location.Cards.GrandBazaarJewelersRoad (grandBazaarJewelersRoad) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Playable
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Charm, Relic))
import Arkham.Window (defaultWindows)

newtype GrandBazaarJewelersRoad = GrandBazaarJewelersRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarJewelersRoad :: LocationCard GrandBazaarJewelersRoad
grandBazaarJewelersRoad =
  locationWith GrandBazaarJewelersRoad Cards.grandBazaarJewelersRoad 3 (Static 2) connectsToAdjacent

instance HasAbilities GrandBazaarJewelersRoad where
  getAbilities (GrandBazaarJewelersRoad a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here investigateAction_

instance RunMessage GrandBazaarJewelersRoad where
  runMessage msg l@(GrandBazaarJewelersRoad attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure l
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) n -> do
      cards <-
        filterM
          ( getIsPlayableWithResources
              iid
              (attrs.ability 1)
              (max 0 n)
              (UnpaidCost NoAction)
              (defaultWindows iid)
          )
          =<< select (inHandOf ForPlay iid <> basic (mapOneOf CardWithTrait [Charm, Relic]))
      chooseOneM iid do
        withI18n skip_
        targets cards \card -> do
          reduceCostOf attrs card 1
          playCardPayingCost iid card
      pure l
    _ -> GrandBazaarJewelersRoad <$> liftRunMessage msg attrs
