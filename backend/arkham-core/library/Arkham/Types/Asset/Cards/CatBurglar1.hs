module Arkham.Types.Asset.Cards.CatBurglar1
  ( CatBurglar1(..)
  , catBurglar1
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype CatBurglar1 = CatBurglar1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catBurglar1 :: AssetCard CatBurglar1
catBurglar1 = ally CatBurglar1 Cards.catBurglar1 (2, 2)

instance HasModifiersFor env CatBurglar1 where
  getModifiersFor _ (InvestigatorTarget iid) (CatBurglar1 a) =
    pure $ toModifiers a [ SkillModifier SkillAgility 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions CatBurglar1 where
  getActions (CatBurglar1 x) =
    [ (restrictedAbility
        x
        1
        (OwnsThis <> AnyRestriction
          [EnemyExists EnemyEngagedWithYou, LocationExists AccessibleLocation]
        )
        (ActionAbility Nothing $ Costs [ActionCost 1, ExhaustThis])
      )
        { abilityDoesNotProvokeAttacksOfOpportunity = True
        }
    ]

instance AssetRunner env => RunMessage env CatBurglar1 where
  runMessage msg (CatBurglar1 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push $ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      CatBurglar1 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      engagedEnemyIds <- getSetList iid
      locationId <- getId @LocationId iid
      accessibleLocationIds <- map unAccessibleLocationId
        <$> getSetList locationId
      pushAll
        $ [ DisengageEnemy iid eid | eid <- engagedEnemyIds ]
        <> [ chooseOne
               iid
               [ MoveAction iid lid Free False | lid <- accessibleLocationIds ]
           | notNull accessibleLocationIds
           ]
      pure $ CatBurglar1 $ attrs & exhaustedL .~ True
    _ -> CatBurglar1 <$> runMessage msg attrs
