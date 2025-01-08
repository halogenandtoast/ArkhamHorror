module Arkham.Asset.Assets.AliceLuxley (aliceLuxley, AliceLuxley (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

newtype AliceLuxley = AliceLuxley AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aliceLuxley :: AssetCard AliceLuxley
aliceLuxley = ally AliceLuxley Cards.aliceLuxley (2, 2)

instance HasModifiersFor AliceLuxley where
  getModifiersFor (AliceLuxley a) = case a.controller of
    Just iid -> modified_ a iid [SkillModifier #intellect 1]
    Nothing -> pure mempty

instance HasAbilities AliceLuxley where
  getAbilities (AliceLuxley a) =
    [ controlledAbility
        a
        1
        (exists (EnemyAt YourLocation <> EnemyCanBeDamagedBySource (toAbilitySource a 1)) <> CanDealDamage)
        $ ReactionAbility (Matcher.DiscoverClues #after You Anywhere $ atLeast 1) (exhaust a)
    ]

instance RunMessage AliceLuxley where
  runMessage msg a@(AliceLuxley attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemies <-
        select
          $ EnemyAt (locationWithInvestigator iid)
          <> EnemyCanBeDamagedBySource (toAbilitySource attrs 1)
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 1]
          | enemy <- enemies
          ]
      pure a
    _ -> AliceLuxley <$> runMessage msg attrs
