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
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype EarlSawyer = EarlSawyer AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earlSawyer :: AssetCard EarlSawyer
earlSawyer = ally EarlSawyer Cards.earlSawyer (3, 2)

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility attrs 1 $ LegacyReactionAbility $ ExhaustCost (toTarget attrs)

instance HasAbilities env EarlSawyer where
  getAbilities iid (AfterEnemyEvaded who _) (EarlSawyer attrs) | iid == who =
    pure [ ability attrs | ownedBy attrs iid ]
  getAbilities iid window (EarlSawyer attrs) = getAbilities iid window attrs

instance HasModifiersFor env EarlSawyer where
  getModifiersFor _ (InvestigatorTarget iid) (EarlSawyer a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env EarlSawyer where
  runMessage msg a@(EarlSawyer attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> EarlSawyer <$> runMessage msg attrs
