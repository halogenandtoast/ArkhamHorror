{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.SpectralMist where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype SpectralMist = SpectralMist Attrs
  deriving newtype (Show, ToJSON, FromJSON)

spectralMist :: TreacheryId -> a -> SpectralMist
spectralMist uuid _ = SpectralMist $ baseAttrs uuid "81025"

instance TreacheryRunner env => HasModifiersFor env SpectralMist where
  getModifiersFor (SkillTestSource iid _ _) _ (SpectralMist Attrs {..}) = do
    lid <- asks $ getId @LocationId iid
    pure [ Difficulty 1 | Just lid == treacheryAttachedLocation ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env SpectralMist where
  getActions iid NonFast (SpectralMist Attrs {..}) = do
    investigatorLocationId <- asks $ getId @LocationId iid
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList treacheryTraits)
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 1 Nothing))
      | Just investigatorLocationId
        == treacheryAttachedLocation
        && hasActionsRemaining
      ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env SpectralMist where
  runMessage msg t@(SpectralMist attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptLocations <- asks
        (getSet @LocationId (TreacheryCardCode $ CardCode "81025"))
      targetLocations <-
        asks $ setToList . (`difference` exemptLocations) . getSet @LocationId
          [Bayou]
      if null targetLocations
        then unshiftMessage (Discard (toTarget attrs))
        else unshiftMessage $ chooseOne
          iid
          [ AttachTreachery treacheryId (LocationTarget x)
          | x <- targetLocations
          ]
      SpectralMist <$> runMessage msg attrs
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId -> do
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          (TreacheryTarget treacheryId)
          Nothing
          SkillIntellect
          2
          [Discard (TreacheryTarget treacheryId)]
          mempty
          mempty
          mempty
        )
    Discard (TreacheryTarget tid) | tid == treacheryId -> do
      unshiftMessages
        [ RemoveAllModifiersOnTargetFrom
            (LocationTarget $ fromJustNote
              "had to have been attached"
              treacheryAttachedLocation
            )
            (TreacherySource treacheryId)
        ]
      SpectralMist <$> runMessage msg attrs
    _ -> SpectralMist <$> runMessage msg attrs
