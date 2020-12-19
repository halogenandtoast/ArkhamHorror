{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Haunted where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype Haunted = Haunted Attrs
  deriving newtype (Show, ToJSON, FromJSON)

haunted :: TreacheryId -> Maybe InvestigatorId -> Haunted
haunted uuid iid = Haunted $ weaknessAttrs uuid iid "01098"

instance HasModifiersFor env Haunted where
  getModifiersFor _ (InvestigatorTarget iid) (Haunted attrs) =
    pure [ AnySkillValue (-1) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Haunted where
  getActions iid NonFast (Haunted a@Attrs {..}) =
    withTreacheryInvestigator a $ \tormented -> do
      investigatorLocationId <- getId @LocationId iid
      treacheryLocation <- getId tormented
      canAffordActions <- getCanAffordCost
        iid
        (toSource a)
        (ActionCost 2 Nothing treacheryTraits)
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 2 Nothing)
            )
        | treacheryLocation == investigatorLocationId && canAffordActions
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Haunted where
  runMessage msg t@(Haunted attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Haunted <$> runMessage msg attrs
