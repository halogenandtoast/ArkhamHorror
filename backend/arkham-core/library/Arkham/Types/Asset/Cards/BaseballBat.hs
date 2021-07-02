module Arkham.Types.Asset.Cards.BaseballBat
  ( BaseballBat(..)
  , baseballBat
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
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target

newtype BaseballBat = BaseballBat AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

baseballBat :: AssetCard BaseballBat
baseballBat = assetWith BaseballBat Cards.baseballBat (slotsL .~ [HandSlot, HandSlot])

instance HasModifiersFor env BaseballBat where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) (InvestigatorTarget iid) (BaseballBat a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [DamageDealt 1]
  getModifiersFor _ _ _ = pure []

fightAbility :: AssetAttrs -> Ability
fightAbility AssetAttrs { assetId } = mkAbility
  (AssetSource assetId)
  1
  (ActionAbility (Just Action.Fight) (ActionCost 1))

instance ActionRunner env  => HasActions env BaseballBat where
  getActions iid window (BaseballBat a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
  getActions _ _ _ = pure []


instance (AssetRunner env) => RunMessage env BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 2])
          source
          (InvestigatorTarget iid)
        , CreateEffect "01074" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> BaseballBat <$> runMessage msg attrs
