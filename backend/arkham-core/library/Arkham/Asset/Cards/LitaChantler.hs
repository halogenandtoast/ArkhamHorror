module Arkham.Asset.Cards.LitaChantler where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

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

