module Arkham.Asset.Cards.DiscOfItzamna2 where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype DiscOfItzamna2 = DiscOfItzamna2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna2 :: AssetCard DiscOfItzamna2
discOfItzamna2 = asset DiscOfItzamna2 Cards.discOfItzamna2

instance HasAbilities DiscOfItzamna2 where
  getAbilities (DiscOfItzamna2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (EnemySpawns Timing.When YourLocation NonEliteEnemy)
            Free
    ]

instance RunMessage DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      push
        (skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 1)
        )
      DiscOfItzamna2 <$> runMessage msg attrs
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      menemySpawnMessage <- fromQueue
        $ find ((== Just EnemySpawnMessage) . messageType)
      a <$ case menemySpawnMessage of
        Just msg'@(EnemySpawn _ _ eid) -> replaceMessage
          msg'
          [Discard (toTarget attrs), Discard (EnemyTarget eid)]
        _ -> pure ()
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
