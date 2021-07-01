module Arkham.Types.Asset.Cards.FortyFiveAutomatic
  ( FortyFiveAutomatic(..)
  , fortyFiveAutomatic
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype FortyFiveAutomatic = FortyFiveAutomatic AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic :: AssetCard FortyFiveAutomatic
fortyFiveAutomatic = hand FortyFiveAutomatic Cards.fortyFiveAutomatic

instance HasModifiersFor env FortyFiveAutomatic where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env FortyFiveAutomatic where
  getActions iid window (FortyFiveAutomatic a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility
              (Just Action.Fight)
              (Costs [ActionCost 1, UseCost (toId a) Ammo 1])
            )
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env FortyFiveAutomatic where
  runMessage msg a@(FortyFiveAutomatic attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      FortyFiveAutomatic <$> runMessage msg (attrs & usesL .~ Uses Ammo 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers
          $ toModifiers attrs [DamageDealt 1, SkillModifier SkillCombat 1]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> FortyFiveAutomatic <$> runMessage msg attrs
