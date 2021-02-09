module Arkham.Types.Asset.Cards.Switchblade
  ( Switchblade(..)
  , switchblade
  )
where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Switchblade = Switchblade AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade :: AssetId -> Switchblade
switchblade uuid =
  Switchblade $ (baseAttrs uuid "01044") { assetSlots = [HandSlot] }

instance HasModifiersFor env Switchblade where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Switchblade where
  getActions iid window (Switchblade a) | ownedBy a iid = do
    let
      ability = mkAbility
        (toSource a)
        1
        (ActionAbility (Just Action.Fight) (ActionCost 1))
    fightAvailable <- hasFightActions iid window
    pure [ ActivateCardAbilityAction iid ability | fightAvailable ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Switchblade where
  runMessage msg a@(Switchblade attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (ChooseFightEnemy iid source SkillCombat False)
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget{} _ n
      | n > 2 && isSource attrs source
      -> a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> Switchblade <$> runMessage msg attrs
