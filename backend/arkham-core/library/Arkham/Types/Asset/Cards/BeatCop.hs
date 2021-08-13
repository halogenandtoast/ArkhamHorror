module Arkham.Types.Asset.Cards.BeatCop
  ( BeatCop(..)
  , beatCop
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
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

instance HasActions BeatCop where
  getActions (BeatCop x) =
    [ restrictedAbility x 1 (OwnsThis <> EnemyExists (EnemyAt YourLocation))
        $ FastAbility (DiscardCost $ toTarget x)
    ]

-- | See: PlayerCardWithBehavior
instance AssetRunner env => RunMessage env BeatCop where
  runMessage msg a@(BeatCop attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      locationEnemyIds <- getSetList locationId
      a
        <$ push
             (chooseOne
               iid
               [ EnemyDamage eid iid source 1 | eid <- locationEnemyIds ]
             )
    _ -> BeatCop <$> runMessage msg attrs
