module Arkham.Asset.Assets.CatBurglar1 (CatBurglar1 (..), catBurglar1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude

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
        $ controlledAbility a 1 (oneOf [exists EnemyEngagedWithYou, CanMoveTo ConnectedLocation])
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage CatBurglar1 where
  runMessage msg (CatBurglar1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      engagedEnemyIds <- select $ enemyEngagedWith iid
      canDisengage <- iid <=~> InvestigatorCanDisengage
      accessibleLocationIds <- getAccessibleLocations iid attrs
      player <- getPlayer iid
      pushAll
        $ [DisengageEnemy iid eid | canDisengage, eid <- engagedEnemyIds]
        <> [ chooseOne player $ targetLabels accessibleLocationIds (only . Move . move attrs iid)
           | notNull accessibleLocationIds
           ]
      pure $ CatBurglar1 $ attrs & exhaustedL .~ True
    _ -> CatBurglar1 <$> runMessage msg attrs
