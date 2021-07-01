module Arkham.Types.Asset.Cards.Machete
  ( Machete(..)
  , machete
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Machete = Machete AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = hand Machete Cards.machete

instance HasModifiersFor env Machete where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Machete where
  getActions iid window (Machete a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Fight) (ActionCost 1))
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Machete where
  runMessage msg a@(Machete attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      criteriaMet <- (== 1) . unEnemyCount <$> getCount iid
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers
            attrs
            ([ DamageDealt 1 | criteriaMet ] <> [SkillModifier SkillCombat 1])
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> Machete <$> runMessage msg attrs
