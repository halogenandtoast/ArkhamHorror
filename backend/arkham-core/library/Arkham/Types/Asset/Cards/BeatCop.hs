module Arkham.Types.Asset.Cards.BeatCop
  ( BeatCop(..)
  , beatCop
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

newtype BeatCop = BeatCop AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop :: AssetCard BeatCop
beatCop = ally BeatCop Cards.beatCop (2, 2)

instance HasModifiersFor env BeatCop where
  getModifiersFor _ (InvestigatorTarget iid) (BeatCop a) =
    pure $ toModifiers a [ SkillModifier SkillCombat 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BeatCop where
  getAbilities (BeatCop x) =
    [ restrictedAbility
          x
          1
          (OwnsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ FastAbility (DiscardCost $ toTarget x)
    ]

instance AssetRunner env => RunMessage env BeatCop where
  runMessage msg a@(BeatCop attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemies <- selectList (EnemyAt YourLocation)
      a <$ push
        (chooseOrRunOne
          iid
          [ EnemyDamage eid iid source NonAttackDamageEffect 1
          | eid <- enemies
          ]
        )
    _ -> BeatCop <$> runMessage msg attrs
