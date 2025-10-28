module Arkham.Asset.Assets.LitaChantler (litaChantler) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (attackingInvestigator)
import Arkham.Matcher
import Arkham.Trait

newtype LitaChantler = LitaChantler AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litaChantler :: AssetCard LitaChantler
litaChantler = ally LitaChantler Cards.litaChantler (3, 3)

instance HasModifiersFor LitaChantler where
  getModifiersFor (LitaChantler a) = for_ a.controller \iid -> do
    modifySelect a (InvestigatorAt $ locationWithInvestigator iid) [SkillModifier #combat 1]

instance HasAbilities LitaChantler where
  getAbilities (LitaChantler a) =
    [ playerLimit PerTestOrAbility
        $ restricted a 1 ControlsThis
        $ freeReaction
        $ EnemyAttackedSuccessfully #when (at_ YourLocation) AnySource (withTrait Monster)
    ]

instance RunMessage LitaChantler where
  runMessage msg a@(LitaChantler attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (attackingInvestigator -> iid) _ -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (DamageDealt 1)
      pure a
    _ -> LitaChantler <$> liftRunMessage msg attrs
