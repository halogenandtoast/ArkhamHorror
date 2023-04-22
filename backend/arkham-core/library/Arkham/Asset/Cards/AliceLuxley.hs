module Arkham.Asset.Cards.AliceLuxley
  ( aliceLuxley
  , AliceLuxley(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype AliceLuxley = AliceLuxley AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aliceLuxley :: AssetCard AliceLuxley
aliceLuxley = ally AliceLuxley Cards.aliceLuxley (2, 2)

instance HasModifiersFor AliceLuxley where
  getModifiersFor (InvestigatorTarget iid) (AliceLuxley a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance HasAbilities AliceLuxley where
  getAbilities (AliceLuxley a) =
    [ restrictedAbility a 1
        ( ControlsThis
        <> EnemyCriteria
          (EnemyExists
            $ EnemyAt YourLocation
            <> EnemyCanBeDamagedBySource (toAbilitySource a 1)
          )
        )
      $ ReactionAbility
        (Matcher.DiscoverClues Timing.After You Anywhere $ AtLeast $ Static 1)
        (ExhaustCost $ toTarget a)
    ]

instance RunMessage AliceLuxley where
  runMessage msg a@(AliceLuxley attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemies <- selectList
        $ EnemyAt (locationWithInvestigator iid)
        <> EnemyCanBeDamagedBySource (toAbilitySource attrs 1)
      push $ chooseOrRunOne iid
        [ targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 1]
        | enemy <- enemies
        ]
      pure a
    _ -> AliceLuxley <$> runMessage msg attrs
