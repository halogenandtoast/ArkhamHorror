module Arkham.Asset.Cards.LitaChantler where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Placement
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype LitaChantler = LitaChantler AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litaChantler :: AssetCard LitaChantler
litaChantler =
  allyWith LitaChantler Cards.litaChantler (3, 3) (isStoryL .~ True)

instance HasModifiersFor LitaChantler where
  getModifiersFor (InvestigatorTarget iid) (LitaChantler a@AssetAttrs {..}) =
    do
      case assetPlacement of
        InPlayArea controllerId -> do
          sameLocation <-
            selectAny
            $ LocationWithInvestigator (InvestigatorWithId iid)
            <> LocationWithInvestigator (InvestigatorWithId controllerId)
          pure [ toModifier a (SkillModifier SkillCombat 1) | sameLocation ]
        _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities LitaChantler where
  getAbilities (LitaChantler a) =
    [ restrictedAbility
        a
        1
        ControlsThis
        (ReactionAbility
          (SkillTestResult
            Timing.When
            (InvestigatorAt YourLocation)
            (WhileAttackingAnEnemy $ EnemyWithTrait Monster)
            (SuccessResult AnyValue)
          )
          Free
        )
    ]

instance RunMessage LitaChantler where
  runMessage msg a@(LitaChantler attrs) = case msg of
    UseCardAbility _ source 1 [Window Timing.When (Window.SuccessfulAttackEnemy _ enemyId _)] _
      | isSource attrs source
      -> do
        a <$ push
          (skillTestModifier attrs (EnemyTarget enemyId) (DamageTaken 1))
    _ -> LitaChantler <$> runMessage msg attrs

