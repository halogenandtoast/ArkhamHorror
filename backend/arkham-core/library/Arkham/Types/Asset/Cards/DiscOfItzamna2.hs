module Arkham.Types.Asset.Cards.DiscOfItzamna2 where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype DiscOfItzamna2 = DiscOfItzamna2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna2 :: AssetCard DiscOfItzamna2
discOfItzamna2 = accessory DiscOfItzamna2 Cards.discOfItzamna2

instance HasActions DiscOfItzamna2 where
  getActions (DiscOfItzamna2 a) =
    [ restrictedAbility a 1 OwnsThis
        $ ReactionAbility
            (EnemySpawns Timing.When YourLocation NonEliteEnemy)
            Free
    ]

instance (AssetRunner env) => RunMessage env DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push
        (skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 1)
        )
      DiscOfItzamna2 <$> runMessage msg attrs
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      menemySpawnMessage <- fromQueue
        $ find ((== Just EnemySpawnMessage) . messageType)
      a <$ case menemySpawnMessage of
        Just msg'@(EnemySpawn _ _ eid) -> replaceMessage
          msg'
          [Discard (toTarget attrs), Discard (EnemyTarget eid)]
        _ -> pure ()
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
