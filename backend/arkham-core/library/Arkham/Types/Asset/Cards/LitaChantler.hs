module Arkham.Types.Asset.Cards.LitaChantler where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype LitaChantler = LitaChantler AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litaChantler :: AssetCard LitaChantler
litaChantler =
  allyWith LitaChantler Cards.litaChantler (3, 3) (isStoryL .~ True)

instance HasId LocationId env InvestigatorId => HasModifiersFor env LitaChantler where
  getModifiersFor _ (InvestigatorTarget iid) (LitaChantler a@AssetAttrs {..}) =
    do
      locationId <- getId @LocationId iid
      case assetInvestigator of
        Nothing -> pure []
        Just ownerId -> do
          sameLocation <- (== locationId) <$> getId ownerId
          pure [ toModifier a (SkillModifier SkillCombat 1) | sameLocation ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities LitaChantler where
  getAbilities (LitaChantler a) =
    [ restrictedAbility
        a
        1
        OwnsThis
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

instance AssetRunner env => RunMessage env LitaChantler where
  runMessage msg a@(LitaChantler attrs) = case msg of
    UseCardAbility _ source [Window Timing.When (Window.SuccessfulAttackEnemy _ enemyId _)] 1 _
      | isSource attrs source
      -> do
        a <$ push
          (skillTestModifier attrs (EnemyTarget enemyId) (DamageTaken 1))
    _ -> LitaChantler <$> runMessage msg attrs

