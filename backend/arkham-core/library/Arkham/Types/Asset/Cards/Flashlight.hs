module Arkham.Types.Asset.Cards.Flashlight
  ( Flashlight(..)
  , flashlight
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

newtype Flashlight = Flashlight AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashlight :: AssetCard Flashlight
flashlight = hand Flashlight Cards.flashlight

instance HasModifiersFor env Flashlight where
  getModifiersFor = noModifiersFor

investigateAbility :: AssetAttrs -> Ability
investigateAbility attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility
    (Just Action.Investigate)
    (Costs [ActionCost 1, UseCost (toId attrs) Supply 1])
  )

instance ActionRunner env => HasActions env Flashlight where
  getActions iid window (Flashlight a) | ownedBy a iid = do
    investigateAvailable <- hasInvestigateActions iid window
    pure
      [ ActivateCardAbilityAction iid (investigateAbility a)
      | investigateAvailable
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Flashlight where
  runMessage msg a@(Flashlight attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Flashlight <$> runMessage msg (attrs & usesL .~ Uses Supply 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      a <$ unshiftMessages
        [ CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [ShroudModifier (-2)])
          source
          (LocationTarget lid)
        , Investigate iid lid source SkillIntellect False
        ]
    _ -> Flashlight <$> runMessage msg attrs
