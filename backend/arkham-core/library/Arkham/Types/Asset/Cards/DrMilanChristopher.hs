module Arkham.Types.Asset.Cards.DrMilanChristopher where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype DrMilanChristopher = DrMilanChristopher AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMilanChristopher :: AssetId -> DrMilanChristopher
drMilanChristopher uuid =
  DrMilanChristopher
    $ baseAttrs uuid "01033"
    & (healthL ?~ 1)
    & (sanityL ?~ 2)
    & (slotsL .~ [AllySlot])

instance HasModifiersFor env DrMilanChristopher where
  getModifiersFor _ (InvestigatorTarget iid) (DrMilanChristopher a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env DrMilanChristopher where
  getActions i (AfterSuccessfulInvestigation You _) (DrMilanChristopher x)
    | ownedBy x i = pure
      [ ActivateCardAbilityAction
          i
          (mkAbility (toSource x) 1 (ReactionAbility Free))
      ]
  getActions i window (DrMilanChristopher x) = getActions i window x

instance AssetRunner env => RunMessage env DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> DrMilanChristopher <$> runMessage msg attrs
