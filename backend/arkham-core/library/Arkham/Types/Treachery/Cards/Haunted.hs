{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Haunted where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype Haunted = Haunted Attrs
  deriving newtype (Show, ToJSON, FromJSON)

haunted :: TreacheryId -> Maybe InvestigatorId -> Haunted
haunted uuid iid = Haunted $ weaknessAttrs uuid iid "01098"

instance (ActionRunner env investigator) => HasActions env investigator Haunted where
  getActions i NonFast (Haunted Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just tormented -> do
        treacheryLocation <- asks (getId tormented)
        pure
          [ ActivateCardAbilityAction
              (getId () i)
              (mkAbility
                (TreacherySource treacheryId)
                1
                (ActionAbility 2 Nothing)
              )
          | treacheryLocation == locationOf i
          ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Haunted where
  runMessage msg t@(Haunted attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage $ AttachTreacheryToInvestigator tid iid
      Haunted <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    AttachTreacheryToInvestigator tid iid | tid == treacheryId -> do
      unshiftMessage
        (AddModifier
          (InvestigatorTarget iid)
          (TreacherySource tid)
          (AnySkillValue (-1))
        )
      Haunted <$> runMessage msg attrs
    UseCardAbility _ _ (TreacherySource tid) 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Haunted <$> runMessage msg attrs
