module Arkham.Asset.Cards.CatBurglar1
  ( CatBurglar1(..)
  , catBurglar1
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( MoveAction )
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype CatBurglar1 = CatBurglar1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catBurglar1 :: AssetCard CatBurglar1
catBurglar1 = ally CatBurglar1 Cards.catBurglar1 (2, 2)

instance HasModifiersFor CatBurglar1 where
  getModifiersFor (InvestigatorTarget iid) (CatBurglar1 a) =
    pure $ toModifiers a [ SkillModifier SkillAgility 1 | controlledBy a iid ]
  getModifiersFor _ _ = pure []


instance HasAbilities CatBurglar1 where
  getAbilities (CatBurglar1 a) =
    [ (restrictedAbility
        a
        1
        (ControlsThis <> AnyCriterion
          [ EnemyCriteria $ EnemyExists EnemyEngagedWithYou
          , LocationExists AccessibleLocation
          ]
        )
        (ActionAbility Nothing $ Costs [ActionCost 1, ExhaustCost (toTarget a)])
      )
        { abilityDoesNotProvokeAttacksOfOpportunity = True
        }
    ]

instance RunMessage CatBurglar1 where
  runMessage msg (CatBurglar1 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      push $ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      CatBurglar1 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      engagedEnemyIds <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId
        iid
      canDisengage <- iid <=~> InvestigatorCanDisengage
      locationId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      accessibleLocationIds <- selectList $ AccessibleFrom $ LocationWithId
        locationId
      pushAll
        $ [ DisengageEnemy iid eid | canDisengage, eid <- engagedEnemyIds ]
        <> [ chooseOne
               iid
               [ targetLabel lid [MoveAction iid lid Free False]
               | lid <- accessibleLocationIds
               ]
           | notNull accessibleLocationIds
           ]
      pure $ CatBurglar1 $ attrs & exhaustedL .~ True
    _ -> CatBurglar1 <$> runMessage msg attrs
