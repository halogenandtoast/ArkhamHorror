module Arkham.Types.Asset.Cards.AlchemicalConcoction
  ( alchemicalConcoction
  , AlchemicalConcoction(..)
  ) where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetId -> AlchemicalConcoction
alchemicalConcoction uuid = AlchemicalConcoction $ baseAttrs uuid "02059"

instance ActionRunner env => HasActions env AlchemicalConcoction where
  getActions iid window (AlchemicalConcoction a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Fight) $ ActionCost 1)
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance (HasId CardCode env EnemyId, HasTarget ForSkillTest env) => HasModifiersFor env AlchemicalConcoction where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) _ (AlchemicalConcoction a)
    | isSource a source
    = do
      skillTestTarget <-
        asks $ fromJustNote "not a skilltest" . getTarget ForSkillTest
      case skillTestTarget of
        EnemyTarget eid -> do
          cardCode <- getId eid
          pure $ toModifiers a [ DamageDealt 6 | cardCode == CardCode "02059" ]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillWillpower False
        ]
    _ -> AlchemicalConcoction <$> runMessage msg attrs
