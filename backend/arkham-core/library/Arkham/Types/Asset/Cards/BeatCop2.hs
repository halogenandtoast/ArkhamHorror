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
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype BeatCop2 = BeatCop2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop2 :: AssetCard BeatCop2
beatCop2 = ally BeatCop2 Cards.beatCop2 (3, 2)

instance HasModifiersFor env BeatCop2 where
  getModifiersFor _ (InvestigatorTarget iid) (BeatCop2 a) =
    pure [ toModifier a (SkillModifier SkillCombat 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

ability :: AssetAttrs -> Ability
ability a = mkAbility
  (toSource a)
  1
  (FastAbility
  $ Costs [ExhaustCost (toTarget a), DamageCost (toSource a) (toTarget a) 1]
  )

instance HasActions env BeatCop2 where
  getActions iid _ (BeatCop2 a) | ownedBy a iid =
    pure [ActivateCardAbilityAction iid (ability a)]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env BeatCop2 where
  runMessage msg a@(BeatCop2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- getSetList locationId
      a <$ unshiftMessage
        (chooseOne
          iid
          [ EnemyDamage eid iid (toSource attrs) 1 | eid <- locationEnemyIds ]
        )
    _ -> BeatCop2 <$> runMessage msg attrs
