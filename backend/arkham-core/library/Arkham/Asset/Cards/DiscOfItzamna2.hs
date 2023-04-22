module Arkham.Asset.Cards.DiscOfItzamna2 where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
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
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- this does not cancel so we must remove manually
      menemySpawnMessage <- fromQueue
        $ find ((== Just EnemySpawnMessage) . messageType)
      a <$ case menemySpawnMessage of
        Just msg'@(EnemySpawn _ _ eid) -> replaceMessage
          msg'
          [Discard (toAbilitySource attrs 1) (toTarget attrs), Discard (toAbilitySource attrs 1) (EnemyTarget eid)]
        _ -> pure ()
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
