module Arkham.Asset.Assets.LitaChantler where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (attackedEnemy)
import Arkham.Matcher
import Arkham.Trait

newtype LitaChantler = LitaChantler AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litaChantler :: AssetCard LitaChantler
litaChantler = allyWith LitaChantler Cards.litaChantler (3, 3) (isStoryL .~ True)

instance HasModifiersFor LitaChantler where
  getModifiersFor (InvestigatorTarget iid) (LitaChantler a) = maybeModified a do
    controllerId <- hoistMaybe a.controller
    liftGuardM $ selectAny $ locationWithInvestigator iid <> locationWithInvestigator controllerId
    pure [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities LitaChantler where
  getAbilities (LitaChantler a) =
    [ restricted a 1 ControlsThis
        $ freeReaction
        $ EnemyAttackedSuccessfully #when (InvestigatorAt YourLocation) AnySource (withTrait Monster)
    ]

instance RunMessage LitaChantler where
  runMessage msg a@(LitaChantler attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (attackedEnemy -> enemyId) _ -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs enemyId (DamageTaken 1)
      pure a
    _ -> LitaChantler <$> liftRunMessage msg attrs
