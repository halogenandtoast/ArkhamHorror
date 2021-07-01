module Arkham.Types.Asset.Cards.DrFrancisMorgan
  ( drFrancisMorgan
  , DrFrancisMorgan(..)
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

newtype DrFrancisMorgan = DrFrancisMorgan AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drFrancisMorgan :: AssetCard DrFrancisMorgan
drFrancisMorgan = ally DrFrancisMorgan Cards.drFrancisMorgan (4, 1)

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility $ ExhaustCost (toTarget attrs))

instance HasActions env DrFrancisMorgan where
  getActions iid (AfterEnemyDefeated You _) (DrFrancisMorgan attrs) =
    pure [ ActivateCardAbilityAction iid (ability attrs) | ownedBy attrs iid ]
  getActions iid window (DrFrancisMorgan attrs) = getActions iid window attrs

instance HasModifiersFor env DrFrancisMorgan where
  getModifiersFor _ (InvestigatorTarget iid) (DrFrancisMorgan a) =
    pure [ toModifier a (SkillModifier SkillCombat 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env DrFrancisMorgan where
  runMessage msg a@(DrFrancisMorgan attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> DrFrancisMorgan <$> runMessage msg attrs
