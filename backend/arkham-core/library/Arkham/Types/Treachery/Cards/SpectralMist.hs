{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.SpectralMist
  ( SpectralMist(..)
  , spectralMist
  )
where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype SpectralMist = SpectralMist Attrs
  deriving newtype (Show, ToJSON, FromJSON)

spectralMist :: TreacheryId -> a -> SpectralMist
spectralMist uuid _ = SpectralMist $ baseAttrs uuid "81025"

instance HasId LocationId env InvestigatorId => HasModifiersFor env SpectralMist where
  getModifiersFor (SkillTestSource iid _ _ _) _ (SpectralMist a) = do
    lid <- getId @LocationId iid
    pure $ toModifiers a [ Difficulty 1 | treacheryOnLocation lid a ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env SpectralMist where
  getActions iid NonFast (SpectralMist a@Attrs {..}) = do
    investigatorLocationId <- getId @LocationId iid
    canAffordActions <- getCanAffordCost iid (toSource a) Nothing (ActionCost 1)
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
      | treacheryOnLocation investigatorLocationId a && canAffordActions
      ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env SpectralMist where
  runMessage msg t@(SpectralMist attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptLocations <- getSet @LocationId
        (TreacheryCardCode $ CardCode "81025")
      targetLocations <-
        setToList . (`difference` exemptLocations) <$> getSet @LocationId
          [Bayou]
      if null targetLocations
        then unshiftMessage (Discard (toTarget attrs))
        else unshiftMessage $ chooseOne
          iid
          [ AttachTreachery treacheryId (LocationTarget x)
          | x <- targetLocations
          ]
      SpectralMist <$> runMessage msg attrs
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          (TreacheryTarget treacheryId)
          Nothing
          SkillIntellect
          2
        )
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> SpectralMist <$> runMessage msg attrs
