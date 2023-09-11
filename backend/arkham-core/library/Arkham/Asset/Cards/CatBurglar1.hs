module Arkham.Asset.Cards.CatBurglar1 (
  CatBurglar1 (..),
  catBurglar1,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection

newtype CatBurglar1 = CatBurglar1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catBurglar1 :: AssetCard CatBurglar1
catBurglar1 = ally CatBurglar1 Cards.catBurglar1 (2, 2)

instance HasModifiersFor CatBurglar1 where
  getModifiersFor (InvestigatorTarget iid) (CatBurglar1 a) =
    pure $ toModifiers a [SkillModifier #agility 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities CatBurglar1 where
  getAbilities (CatBurglar1 a) =
    [ doesNotProvokeAttacksOfOpportunity
        $ restrictedAbility
          a
          1
          ( ControlsThis
              <> AnyCriterion
                [ EnemyCriteria $ EnemyExists EnemyEngagedWithYou
                , LocationExists AccessibleLocation
                ]
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, exhaust a]
    ]

instance RunMessage CatBurglar1 where
  runMessage msg (CatBurglar1 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == toId attrs -> do
      push $ skillTestModifier attrs iid (SkillModifier #agility 1)
      CatBurglar1 <$> runMessage msg attrs
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      engagedEnemyIds <- selectList $ enemyEngagedWith iid
      canDisengage <- iid <=~> InvestigatorCanDisengage
      locationId <- fieldJust InvestigatorLocation iid
      accessibleLocationIds <- selectList $ AccessibleFrom $ LocationWithId locationId
      pushAll
        $ [DisengageEnemy iid eid | canDisengage, eid <- engagedEnemyIds]
          <> [ chooseOne
              iid
              [ targetLabel lid [Move $ move attrs iid lid]
              | lid <- accessibleLocationIds
              ]
             | notNull accessibleLocationIds
             ]
      pure $ CatBurglar1 $ attrs & exhaustedL .~ True
    _ -> CatBurglar1 <$> runMessage msg attrs
