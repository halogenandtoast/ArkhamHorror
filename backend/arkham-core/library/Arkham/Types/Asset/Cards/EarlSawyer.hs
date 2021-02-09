module Arkham.Types.Asset.Cards.EarlSawyer
  ( earlSawyer
  , EarlSawyer(..)
  )
where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype EarlSawyer = EarlSawyer AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earlSawyer :: AssetId -> EarlSawyer
earlSawyer uuid =
  EarlSawyer
    $ baseAttrs uuid "02218"
    & (healthL ?~ 3)
    & (sanityL ?~ 2)
    & (slotsL .~ [AllySlot])

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility $ ExhaustCost (toTarget attrs))

instance HasActions env EarlSawyer where
  getActions iid (AfterEnemyEvaded You _) (EarlSawyer attrs) =
    pure [ ActivateCardAbilityAction iid (ability attrs) | ownedBy attrs iid ]
  getActions iid window (EarlSawyer attrs) = getActions iid window attrs

instance HasModifiersFor env EarlSawyer where
  getModifiersFor _ (InvestigatorTarget iid) (EarlSawyer a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env EarlSawyer where
  runMessage msg a@(EarlSawyer attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> EarlSawyer <$> runMessage msg attrs
