module Arkham.Types.Asset.Cards.Shrivelling
  ( Shrivelling(..)
  , shrivelling
  ) where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses

newtype Shrivelling = Shrivelling AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

shrivelling :: AssetId -> Shrivelling
shrivelling uuid =
  Shrivelling $ (baseAttrs uuid "01060") { assetSlots = [ArcaneSlot] }

instance HasModifiersFor env Shrivelling where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Shrivelling where
  getActions iid window (Shrivelling a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility
              (Just Action.Fight)
              (Costs [ActionCost 1, UseCost (toId a) Charge 1])
            )
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Shrivelling where
  runMessage msg a@(Shrivelling attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Shrivelling <$> runMessage msg (attrs & usesL .~ Uses Charge 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillWillpower False
        ]
    _ -> Shrivelling <$> runMessage msg attrs
