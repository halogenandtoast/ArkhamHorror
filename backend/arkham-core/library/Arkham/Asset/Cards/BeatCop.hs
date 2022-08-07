module Arkham.Asset.Cards.BeatCop
  ( BeatCop(..)
  , beatCop
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.SkillType
import Arkham.Target

newtype BeatCop = BeatCop AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop :: AssetCard BeatCop
beatCop = ally BeatCop Cards.beatCop (2, 2)

instance HasModifiersFor BeatCop where
  getModifiersFor (InvestigatorTarget iid) (BeatCop a) =
    pure $ toModifiers a [ SkillModifier SkillCombat 1 | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance HasAbilities BeatCop where
  getAbilities (BeatCop x) =
    [ restrictedAbility
          x
          1
          (ControlsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ FastAbility (DiscardCost $ toTarget x)
    ]

instance RunMessage BeatCop where
  runMessage msg a@(BeatCop attrs) = case msg of
    InDiscard _ (UseCardAbility iid source _ 1 _) | isSource attrs source -> do
      enemies <- selectList (EnemyAt YourLocation)
      a <$ push
        (chooseOrRunOne
          iid
          [ EnemyDamage eid iid source NonAttackDamageEffect 1
          | eid <- enemies
          ]
        )
    _ -> BeatCop <$> runMessage msg attrs
