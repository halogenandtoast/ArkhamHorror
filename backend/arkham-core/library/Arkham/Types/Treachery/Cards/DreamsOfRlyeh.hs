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
      unshiftMessages
        [ AttachTreachery treacheryId (InvestigatorTarget iid)
        , AddModifiers
          (InvestigatorTarget iid)
          source
          [SkillModifier SkillWillpower (-1), SanityModifier (-1)]
        ]
      DreamsOfRlyeh <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
          [Discard (TreacheryTarget treacheryId)]
          mempty
          mempty
          mempty
        )
    _ -> DreamsOfRlyeh <$> runMessage msg attrs
