module Arkham.Types.Asset.Cards.Fieldwork
  ( fieldwork
  , Fieldwork(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype Fieldwork = Fieldwork AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldwork :: AssetCard Fieldwork
fieldwork = asset Fieldwork Cards.fieldwork

ability :: AssetAttrs -> Ability
ability a = restrictedAbility
  a
  1
  ClueOnLocation
  (LegacyReactionAbility $ ExhaustCost $ toTarget a)

instance HasAbilities env Fieldwork where
  getAbilities iid (AfterEntering iid' _) (Fieldwork attrs)
    | ownedBy attrs iid && iid == iid' = pure [ability attrs]
  getAbilities iid window (Fieldwork attrs) = getAbilities iid window attrs

instance HasModifiersFor env Fieldwork

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Fieldwork where
  runMessage msg a@(Fieldwork attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a
        <$ push
             (CreateEffect (toCardCode attrs) Nothing source
             $ InvestigatorTarget iid
             )
    _ -> Fieldwork <$> runMessage msg attrs
