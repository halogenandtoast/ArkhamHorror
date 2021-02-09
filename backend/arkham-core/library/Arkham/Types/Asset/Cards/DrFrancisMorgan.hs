module Arkham.Types.Asset.Cards.DrFrancisMorgan
  ( drFrancisMorgan
  , DrFrancisMorgan(..)
  ) where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype DrFrancisMorgan = DrFrancisMorgan AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drFrancisMorgan :: AssetId -> DrFrancisMorgan
drFrancisMorgan uuid = DrFrancisMorgan $ (baseAttrs uuid "02080")
  { assetSlots = [AllySlot]
  , assetHealth = Just 4
  , assetSanity = Just 1
  }

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
