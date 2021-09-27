module Arkham.Types.Asset.Cards.BeatCop2
  ( BeatCop2(..)
  , beatCop2
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype BeatCop2 = BeatCop2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop2 :: AssetCard BeatCop2
beatCop2 = ally BeatCop2 Cards.beatCop2 (3, 2)

instance HasModifiersFor env BeatCop2 where
  getModifiersFor _ (InvestigatorTarget iid) (BeatCop2 a) =
    pure [ toModifier a (SkillModifier SkillCombat 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BeatCop2 where
  getAbilities (BeatCop2 x) =
    [ restrictedAbility
          x
          1
          (OwnsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ FastAbility
        $ Costs
            [ExhaustCost (toTarget x), DamageCost (toSource x) (toTarget x) 1]
    ]

instance AssetRunner env => RunMessage env BeatCop2 where
  runMessage msg a@(BeatCop2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemies <- selectList (EnemyAt YourLocation)
      a <$ push
        (chooseOrRunOne
          iid
          [ EnemyDamage eid iid (toSource attrs) NonAttackDamageEffect 1
          | eid <- enemies
          ]
        )
    _ -> BeatCop2 <$> runMessage msg attrs
