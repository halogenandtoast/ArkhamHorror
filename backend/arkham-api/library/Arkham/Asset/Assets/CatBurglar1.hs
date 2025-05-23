module Arkham.Asset.Assets.CatBurglar1 (catBurglar1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype CatBurglar1 = CatBurglar1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catBurglar1 :: AssetCard CatBurglar1
catBurglar1 = ally CatBurglar1 Cards.catBurglar1 (2, 2)

instance HasModifiersFor CatBurglar1 where
  getModifiersFor (CatBurglar1 a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities CatBurglar1 where
  getAbilities (CatBurglar1 a) =
    [ doesNotProvokeAttacksOfOpportunity
        $ controlled a 1 (oneOf [exists EnemyEngagedWithYou, CanMoveTo ConnectedLocation])
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage CatBurglar1 where
  runMessage msg a@(CatBurglar1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (iid <=~> InvestigatorCanDisengage) do
        selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      locations <- getAccessibleLocations iid attrs
      chooseTargetM iid locations $ moveTo attrs iid
      pure a
    _ -> CatBurglar1 <$> liftRunMessage msg attrs
