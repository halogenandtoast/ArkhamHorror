module Arkham.Types.Asset.Cards.ZebulonWhateley
  ( zebulonWhateley
  , ZebulonWhateley(..)
  )
where

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
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype ZebulonWhateley = ZebulonWhateley AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zebulonWhateley :: AssetCard ZebulonWhateley
zebulonWhateley = allyWith ZebulonWhateley Cards.zebulonWhateley (1, 4) (isStoryL .~ True)

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility $ ExhaustCost (toTarget attrs))

instance HasActions env ZebulonWhateley where
  getActions iid (AfterPassSkillTest _ (TreacherySource _) You _) (ZebulonWhateley attrs)
    = pure [ ActivateCardAbilityAction iid (ability attrs) | ownedBy attrs iid ]
  getActions iid window (ZebulonWhateley attrs) = getActions iid window attrs

instance HasModifiersFor env ZebulonWhateley where
  getModifiersFor _ (InvestigatorTarget iid) (ZebulonWhateley a) =
    pure [ toModifier a (SkillModifier SkillWillpower 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env ZebulonWhateley where
  runMessage msg a@(ZebulonWhateley attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> ZebulonWhateley <$> runMessage msg attrs
