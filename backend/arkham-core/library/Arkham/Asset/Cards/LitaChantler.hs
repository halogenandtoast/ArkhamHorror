module Arkham.Asset.Cards.LitaChantler where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype LitaChantler = LitaChantler AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litaChantler :: AssetCard LitaChantler
litaChantler = allyWith LitaChantler Cards.litaChantler (3, 3) (isStoryL .~ True)

instance HasModifiersFor LitaChantler where
  getModifiersFor (InvestigatorTarget iid) (LitaChantler a) = case a.controller of
    Just controllerId -> do
      sameLocation <- selectAny $ locationWithInvestigator iid <> locationWithInvestigator controllerId
      pure $ toModifiers a [SkillModifier #combat 1 | sameLocation]
    _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities LitaChantler where
  getAbilities (LitaChantler a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ EnemyAttackedSuccessfully #when (InvestigatorAt YourLocation) AnySource (withTrait Monster)
    ]

instance RunMessage LitaChantler where
  runMessage msg a@(LitaChantler attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (attackedEnemy -> enemyId) _ -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs enemyId (DamageTaken 1)
      pure a
    _ -> LitaChantler <$> runMessage msg attrs
