{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.DreamsOfRlyeh where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype DreamsOfRlyeh = DreamsOfRlyeh Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dreamsOfRlyeh :: TreacheryId -> a -> DreamsOfRlyeh
dreamsOfRlyeh uuid _ = DreamsOfRlyeh $ baseAttrs uuid "01182"

instance HasModifiersFor env DreamsOfRlyeh where
  getModifiersFor _ (InvestigatorTarget iid) (DreamsOfRlyeh attrs) =
    pure $ if iid `elem` treacheryAttachedInvestigator attrs
      then [SkillModifier SkillWillpower (-1), SanityModifier (-1)]
      else []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env DreamsOfRlyeh where
  getActions iid NonFast (DreamsOfRlyeh Attrs {..}) = do
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList treacheryTraits)
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 1 Nothing))
      | treacheryAttachedInvestigator == Just iid && hasActionsRemaining
      ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env DreamsOfRlyeh where
  runMessage msg t@(DreamsOfRlyeh attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
        )
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> DreamsOfRlyeh <$> runMessage msg attrs
