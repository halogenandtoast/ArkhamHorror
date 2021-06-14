module Arkham.Types.Asset.Cards.SongOfTheDead2
  ( songOfTheDead2
  , SongOfTheDead2(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: AssetId -> SongOfTheDead2
songOfTheDead2 uuid =
  SongOfTheDead2 . (slotsL .~ [ArcaneSlot]) $ baseAttrs uuid "02112"

instance ActionRunner env => HasActions env SongOfTheDead2 where
  getActions iid window (SongOfTheDead2 a) | ownedBy a iid = do
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

instance HasModifiersFor env SongOfTheDead2 where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      SongOfTheDead2 <$> runMessage msg (attrs & usesL .~ Uses Charge 5)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillWillpower 1])
          source
          (InvestigatorTarget iid)
        , CreateEffect "02112" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillWillpower mempty False
        ]
    _ -> SongOfTheDead2 <$> runMessage msg attrs
