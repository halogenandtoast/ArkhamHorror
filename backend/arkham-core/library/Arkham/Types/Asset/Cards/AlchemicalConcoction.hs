module Arkham.Types.Asset.Cards.AlchemicalConcoction
  ( alchemicalConcoction
  , AlchemicalConcoction(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetCard AlchemicalConcoction
alchemicalConcoction = asset AlchemicalConcoction Cards.alchemicalConcoction

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

instance (HasId CardCode env EnemyId, HasSkillTest env) => HasModifiersFor env AlchemicalConcoction where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) _ (AlchemicalConcoction a)
    | isSource a source
    = do
      skillTestTarget <- fromJustNote "not a skilltest" <$> getSkillTestTarget
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
        , ChooseFightEnemy iid source SkillWillpower mempty False
        ]
    _ -> AlchemicalConcoction <$> runMessage msg attrs
