module Arkham.Types.Asset.Cards.Rolands38Special
  ( Rolands38Special(..)
  , rolands38Special
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
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Rolands38Special = Rolands38Special AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolands38Special :: AssetCard Rolands38Special
rolands38Special = hand Rolands38Special Cards.rolands38Special

instance HasModifiersFor env Rolands38Special where
  getModifiersFor = noModifiersFor

fightAbility :: AssetAttrs -> Ability
fightAbility attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility
    (Just Action.Fight)
    (Costs [ActionCost 1, UseCost (toId attrs) Ammo 1])
  )

instance ActionRunner env => HasActions env Rolands38Special where
  getActions iid window (Rolands38Special a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Rolands38Special where
  runMessage msg a@(Rolands38Special attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Rolands38Special <$> runMessage msg (attrs & usesL .~ Uses Ammo 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      anyClues <- (> 0) . unClueCount <$> getCount locationId
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers
            attrs
            [ DamageDealt 1
            , SkillModifier SkillCombat (if anyClues then 3 else 1)
            ]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> Rolands38Special <$> runMessage msg attrs
