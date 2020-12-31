{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.FortyOneDerringer
  ( FortyOneDerringer(..)
  , fortyOneDerringer
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype FortyOneDerringer = FortyOneDerringer Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fortyOneDerringer :: AssetId -> FortyOneDerringer
fortyOneDerringer uuid =
  FortyOneDerringer $ (baseAttrs uuid "01047") { assetSlots = [HandSlot] }

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
            (ActionAbility (Just Action.Fight) (ActionCost 1))
          )
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env FortyOneDerringer where
  runMessage msg a@(FortyOneDerringer attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      FortyOneDerringer
        <$> runMessage msg (attrs & usesL .~ Uses Resource.Ammo 3)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 2])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
      pure $ FortyOneDerringer $ attrs & usesL %~ Resource.use
    PassedSkillTest iid (Just Action.Fight) source _ n
      | isSource attrs source && n >= 2 -> a <$ unshiftMessage
        (CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> FortyOneDerringer <$> runMessage msg attrs
