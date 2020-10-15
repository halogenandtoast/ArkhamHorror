{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Parlor where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner

newtype Parlor = Parlor Attrs
  deriving newtype (Show, ToJSON, FromJSON)

parlor :: Parlor
parlor =
  Parlor $ (baseAttrs "01115" "Parlor" 2 (Static 0) Diamond [Square] mempty)
    { locationBlocked = True
    }

instance HasModifiersFor env investigator Parlor where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Parlor where
  getActions i NonFast (Parlor attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i NonFast attrs
    maid <- asks (fmap unStoryAssetId <$> getId (CardCode "01117"))
    case maid of
      Nothing -> pure []
      Just aid -> do
        miid <- asks (fmap unOwnerId . getId aid)
        assetLocationId <- asks (getId aid)
        pure
          $ baseActions
          <> [ ActivateCardAbilityAction
                 (getId () i)
                 (mkAbility
                   (LocationSource "01115")
                   1
                   (ActionAbility 1 (Just Action.Resign))
                 )
             | atLocation i attrs
               && hasActionsRemaining i (Just Action.Resign) locationTraits
             ]
          <> [ ActivateCardAbilityAction
                 (getId () i)
                 (mkAbility
                   (ProxySource (AssetSource aid) (LocationSource "01115"))
                   2
                   (ActionAbility 1 (Just Action.Parley))
                 )
             | isNothing miid
               && Just (locationOf i)
               == assetLocationId
               && hasActionsRemaining i (Just Action.Parley) locationTraits
             ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env Parlor where
  runMessage msg l@(Parlor attrs@Attrs {..}) = case msg of
    RevealLocation lid | lid == locationId -> do
      attrs' <- runMessage msg attrs
      pure $ Parlor $ attrs' & blocked .~ False
    UseCardAbility iid source _ 1 | isSource attrs source && locationRevealed ->
      l <$ unshiftMessage (Resign iid)
    UseCardAbility iid (ProxySource _ source) _ 2
      | isSource attrs source && locationRevealed -> do
        maid <- asks (fmap unStoryAssetId <$> getId (CardCode "01117"))
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> l <$ unshiftMessage
            (BeginSkillTest
              iid
              source
              (AssetTarget aid)
              (Just Action.Parley)
              SkillIntellect
              4
              [TakeControlOfAsset iid aid]
              []
              []
              mempty
            )
    _ -> Parlor <$> runMessage msg attrs
