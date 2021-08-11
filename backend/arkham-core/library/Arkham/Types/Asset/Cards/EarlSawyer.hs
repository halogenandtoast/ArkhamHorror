module Arkham.Types.Asset.Cards.EarlSawyer
  ( earlSawyer
  , EarlSawyer(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message hiding (EnemyEvaded)
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype EarlSawyer = EarlSawyer AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earlSawyer :: AssetCard EarlSawyer
earlSawyer = ally EarlSawyer Cards.earlSawyer (3, 2)

instance HasActions EarlSawyer where
  getActions (EarlSawyer attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ReactionAbility (EnemyEvaded Timing.After You AnyEnemy) ExhaustThis
    ]

instance HasModifiersFor env EarlSawyer where
  getModifiersFor _ (InvestigatorTarget iid) (EarlSawyer a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env EarlSawyer where
  runMessage msg a@(EarlSawyer attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> EarlSawyer <$> runMessage msg attrs
