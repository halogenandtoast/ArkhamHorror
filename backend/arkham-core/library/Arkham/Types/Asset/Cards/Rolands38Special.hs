{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Rolands38Special
  ( Rolands38Special(..)
  , rolands38Special
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype Rolands38Special = Rolands38Special Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rolands38Special :: AssetId -> Rolands38Special
rolands38Special uuid =
  Rolands38Special $ (baseAttrs uuid "01006") { assetSlots = [HandSlot] }

instance HasModifiersFor env Rolands38Special where
  getModifiersFor = noModifiersFor

fightAbility :: Attrs -> Ability
fightAbility Attrs { assetId } =
  mkAbility (AssetSource assetId) 1 (ActionAbility 1 (Just Action.Fight))

instance ActionRunner env => HasActions env Rolands38Special where
  getActions iid window (Rolands38Special a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction iid (fightAbility a)
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Rolands38Special where
  runMessage msg (Rolands38Special attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Rolands38Special
        <$> runMessage msg (attrs & usesL .~ Uses Resource.Ammo 4)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- getId @LocationId iid
      anyClues <- (/= 0) . unClueCount <$> getCount locationId
      unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers $ toModifiers
            attrs
            [ DamageDealt 1
            , SkillModifier SkillCombat (if anyClues then 3 else 1)
            ]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
      pure $ Rolands38Special $ attrs & usesL %~ Resource.use
    _ -> Rolands38Special <$> runMessage msg attrs
