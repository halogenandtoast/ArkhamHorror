module Arkham.Asset.Assets.LadyEsprit (ladyEsprit) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LadyEsprit = LadyEsprit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ladyEsprit :: AssetCard LadyEsprit
ladyEsprit = ally LadyEsprit Cards.ladyEsprit (2, 4)

instance HasAbilities LadyEsprit where
  getAbilities (LadyEsprit x) =
    [ restricted
        x
        1
        ( OnSameLocation
            <> any_
              [HealableInvestigator (toSource x) #damage You, You <> InvestigatorCanGainResources]
        )
        (actionAbilityWithCost $ exhaust x <> horrorCost x 1)
    ]

instance RunMessage LadyEsprit where
  runMessage msg a@(LadyEsprit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        whenM (canHaveDamageHealed attrs iid) do
          damageLabeled iid $ healDamage iid (attrs.ability 1) 2
        whenM (can.gain.resources iid) do
          resourceLabeled iid $ gainResources iid (attrs.ability 1) 2
      pure a
    _ -> LadyEsprit <$> liftRunMessage msg attrs
