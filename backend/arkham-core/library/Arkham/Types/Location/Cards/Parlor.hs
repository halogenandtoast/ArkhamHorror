{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Parlor where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import ClassyPrelude
import Lens.Micro

newtype Parlor = Parlor Attrs
  deriving newtype (Show, ToJSON, FromJSON)

parlor :: Parlor
parlor = Parlor $ (baseAttrs "01115" "Parlor" 2 (Static 0) Diamond [Square])
  { locationBlocked = True
  }

instance (ActionRunner env investigator) => HasActions env investigator Parlor where
  getActions i window (Parlor attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i window attrs
    maid <- asks (fmap unStoryAssetId <$> getId (CardCode "01117"))
    case maid of
      Nothing -> pure []
      Just aid -> do
        miid <- fmap unOwnerId <$> asks (getId aid)
        pure
          $ baseActions
          <> [ ActivateCardAbilityAction
                 (getId () i)
                 (mkAbility
                   (LocationSource "01115")
                   1
                   (ActionAbility 1 (Just Action.Resign))
                 )
             | getId () i `elem` locationInvestigators
             ]
          <> [ ActivateCardAbilityAction
                 (getId () i)
                 ((mkAbility
                    (AssetSource aid)
                    1
                    (ActionAbility 1 (Just Action.Parley))
                  )
                   { abilityProvider = LocationSource "01115"
                   }
                 )
             | isNothing miid
             ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env Parlor where
  runMessage msg l@(Parlor attrs@Attrs {..}) = case msg of
    RevealLocation lid | lid == locationId -> do
      attrs' <- runMessage msg attrs
      pure $ Parlor $ attrs' & blocked .~ False
    UseCardAbility iid _ (LocationSource lid) 1
      | lid == locationId && locationRevealed -> l
      <$ unshiftMessage (Resign iid)
    UseCardAbility iid _ (LocationSource lid) 2
      | lid == locationId && locationRevealed -> do
        maid <- asks (fmap unStoryAssetId <$> getId (CardCode "01117"))
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> l <$ unshiftMessage
            (BeginSkillTest
              iid
              (LocationSource lid)
              (Just Action.Parley)
              SkillIntellect
              4
              [TakeControlOfAsset iid aid]
              []
              []
              mempty
            )
    _ -> Parlor <$> runMessage msg attrs
