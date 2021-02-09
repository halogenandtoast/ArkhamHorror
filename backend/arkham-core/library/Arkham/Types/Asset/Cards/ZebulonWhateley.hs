module Arkham.Types.Asset.Cards.ZebulonWhateley
  ( zebulonWhateley
  , ZebulonWhateley(..)
  )
where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype ZebulonWhateley = ZebulonWhateley AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zebulonWhateley :: AssetId -> ZebulonWhateley
zebulonWhateley uuid =
  ZebulonWhateley
    $ baseAttrs uuid "02217"
    & (healthL ?~ 1)
    & (sanityL ?~ 4)
    & (slotsL .~ [AllySlot])

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
