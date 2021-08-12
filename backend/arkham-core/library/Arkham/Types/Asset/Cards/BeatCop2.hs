module Arkham.Types.Asset.Cards.BeatCop2
  ( BeatCop2(..)
  , beatCop2
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
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

instance HasActions BeatCop2 where
  getActions (BeatCop2 x) =
    [ restrictedAbility x 1 (OwnsThis <> EnemyExists EnemyAtYourLocation)
        $ FastAbility
        $ Costs
            [ExhaustCost (toTarget x), DamageCost (toSource x) (toTarget x) 1]
    ]

instance (AssetRunner env) => RunMessage env BeatCop2 where
  runMessage msg a@(BeatCop2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- getSetList locationId
      a <$ push
        (chooseOne
          iid
          [ EnemyDamage eid iid (toSource attrs) 1 | eid <- locationEnemyIds ]
        )
    _ -> BeatCop2 <$> runMessage msg attrs
