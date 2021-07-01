module Arkham.Types.Asset.Cards.FortyOneDerringer
  ( FortyOneDerringer(..)
  , fortyOneDerringer
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

newtype FortyOneDerringer = FortyOneDerringer AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyOneDerringer :: AssetCard FortyOneDerringer
fortyOneDerringer = hand FortyOneDerringer Cards.fortyOneDerringer

instance HasModifiersFor env FortyOneDerringer where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env FortyOneDerringer where
  getActions iid window (FortyOneDerringer a) | ownedBy a iid = do
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

instance AssetRunner env => RunMessage env FortyOneDerringer where
  runMessage msg a@(FortyOneDerringer attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      FortyOneDerringer <$> runMessage msg (attrs & usesL .~ Uses Ammo 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 2])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    PassedSkillTest iid (Just Action.Fight) source _ _ n
      | isSource attrs source && n >= 2 -> a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> FortyOneDerringer <$> runMessage msg attrs
