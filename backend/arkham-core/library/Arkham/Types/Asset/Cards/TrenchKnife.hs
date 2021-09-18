module Arkham.Types.Asset.Cards.TrenchKnife
  ( trenchKnife
  , TrenchKnife(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype TrenchKnife = TrenchKnife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchKnife :: AssetCard TrenchKnife
trenchKnife = hand TrenchKnife Cards.trenchKnife

instance HasModifiersFor env TrenchKnife where
  getModifiersFor _ (InvestigatorTarget iid) (TrenchKnife attrs)
    | attrs `ownedBy` iid = pure $ toModifiers
      attrs
      [ActionDoesNotCauseAttacksOfOpportunity Action.Engage]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TrenchKnife where
  getAbilities (TrenchKnife attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance AssetRunner env => RunMessage env TrenchKnife where
  runMessage msg a@(TrenchKnife attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemyCount <- selectCount EnemyEngagedWithYou
      a <$ pushAll
        [ skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat enemyCount)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> TrenchKnife <$> runMessage msg attrs
