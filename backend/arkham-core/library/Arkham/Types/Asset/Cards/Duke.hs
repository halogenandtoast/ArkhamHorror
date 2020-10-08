{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Duke where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype Duke = Duke Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

duke :: AssetId -> Duke
duke uuid =
  Duke $ (baseAttrs uuid "02014") { assetHealth = Just 2, assetSanity = Just 3 }

instance HasModifiersFor env investigator Duke where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Duke where
  getActions i NonFast (Duke Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      fightAvailable <- hasFightActions i NonFast
      investigateAvailable <- hasInvestigateActions i NonFast
      pure
        $ [ ActivateCardAbilityAction
              (getId () i)
              (mkAbility
                (AssetSource assetId)
                1
                (ActionAbility 1 (Just Action.Fight))
              )
          | fightAvailable && canDo Action.Fight i && not assetExhausted
          ]
        <> [ ActivateCardAbilityAction
               (getId () i)
               (mkAbility
                 (AssetSource assetId)
                 2
                 (ActionAbility 1 (Just Action.Investigate))
               )
           | investigateAvailable
             && canDo Action.Investigate i
             && not assetExhausted
           ]
  getActions i window (Duke x) = getActions i window x

instance (AssetRunner env) => RunMessage env Duke where
  runMessage msg (Duke attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      unshiftMessage
        (ChooseFightEnemy
          iid
          (AssetSource aid)
          SkillCombat
          [BaseSkillOf SkillCombat 4, DamageDealt 1]
          mempty
          False
        )
      pure . Duke $ attrs & exhausted .~ True
    UseCardAbility iid _ (AssetSource aid) _ 2 | aid == assetId -> do
      lid <- asks (getId iid)
      blockedLocationIds <- HashSet.map unBlockedLocationId <$> asks (getSet ())
      connectedLocationIds <- HashSet.map unConnectedLocationId
        <$> asks (getSet lid)
      let
        unblockedConnectedLocationIds =
          setToList $ connectedLocationIds `difference` blockedLocationIds
      let
        investigate atLid = Investigate
          iid
          atLid
          SkillIntellect
          [BaseSkillOf SkillIntellect 4]
          mempty
          mempty
          False
      if null unblockedConnectedLocationIds
        then unshiftMessage $ investigate lid
        else unshiftMessage
          (Ask
            iid
            (ChooseOne
            $ investigate lid
            : [ Run [MoveAction iid lid' False, investigate lid']
              | lid' <- unblockedConnectedLocationIds
              ]
            )
          )
      pure . Duke $ attrs & exhausted .~ True
    _ -> Duke <$> runMessage msg attrs
