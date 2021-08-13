module Arkham.Types.Asset.Cards.LitaChantler where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

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

instance HasActions LitaChantler where
  getActions (LitaChantler a) =
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
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just target ->
          a <$ push (skillTestModifier attrs target (DamageTaken 1))
        Nothing -> error "must be during skill test"
    _ -> LitaChantler <$> runMessage msg attrs

