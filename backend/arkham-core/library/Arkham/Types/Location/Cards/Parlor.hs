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
  , locationAbilities =
    [ mkAbility
        (LocationSource "01115")
        1
        (ActionAbility 1 (Just Action.Resign))
    ]
  }

instance HasActions Parlor where
  getActions (Parlor attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env Parlor where
  runMessage msg l@(Parlor attrs@Attrs {..}) = case msg of
    RevealLocation lid | lid == locationId -> do
      attrs' <- runMessage msg attrs
      pure $ Parlor $ attrs' & blocked .~ False
    PrePlayerWindow | locationRevealed -> do
      aid <- unStoryAssetId <$> asks (getId (CardCode "01117"))
      miid <- fmap unOwnerId <$> asks (getId aid)
      case miid of
        Just _ ->
          l <$ unshiftMessage (RemoveAbilitiesFrom (LocationSource locationId))
        Nothing -> l <$ unshiftMessages
          [ RemoveAbilitiesFrom (LocationSource locationId)
          , AddAbility
            (AssetSource aid)
            ((mkAbility
               (AssetSource aid)
               2
               (ActionAbility 1 (Just Action.Parley))
             )
              { abilityProvider = LocationSource locationId
              }
            )
          ]
    UseCardAbility iid _ (LocationSource lid) 1
      | lid == locationId && locationRevealed -> l
      <$ unshiftMessage (Resign iid)
    UseCardAbility iid _ (LocationSource lid) 2
      | lid == locationId && locationRevealed -> do
        aid <- unStoryAssetId <$> asks (getId (CardCode "01117"))
        l <$ unshiftMessage
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
