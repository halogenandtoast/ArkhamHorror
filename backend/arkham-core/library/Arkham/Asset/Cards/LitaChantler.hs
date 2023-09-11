module Arkham.Asset.Cards.LitaChantler where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype LitaChantler = LitaChantler AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litaChantler :: AssetCard LitaChantler
litaChantler = allyWith LitaChantler Cards.litaChantler (3, 3) (isStoryL .~ True)

instance HasModifiersFor LitaChantler where
  getModifiersFor (InvestigatorTarget iid) (LitaChantler a) = case assetController a of
    Just controllerId -> do
      sameLocation <- selectAny $ locationWithInvestigator iid <> locationWithInvestigator controllerId
      pure $ toModifiers a [SkillModifier #combat 1 | sameLocation]
    _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities LitaChantler where
  getAbilities (LitaChantler a) =
    [ restrictedAbility a 1 ControlsThis
        $ (`ReactionAbility` Free)
        $ SkillTestResult
          Timing.When
          (InvestigatorAt YourLocation)
          (WhileAttackingAnEnemy $ EnemyWithTrait Monster)
          (SuccessResult AnyValue)
    ]

getEnemyId :: [Window] -> EnemyId
getEnemyId [] = error "impossible"
getEnemyId ((windowType -> Window.SuccessfulAttackEnemy _ enemyId _) : _) = enemyId
getEnemyId (_ : rest) = getEnemyId rest

instance RunMessage LitaChantler where
  runMessage msg a@(LitaChantler attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemyId -> enemyId) _ -> do
      push $ skillTestModifier attrs enemyId (DamageTaken 1)
      pure a
    _ -> LitaChantler <$> runMessage msg attrs
