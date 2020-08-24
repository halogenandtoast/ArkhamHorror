{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Hypochondria where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype Hypochondria = Hypochondria Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hypochondria :: TreacheryId -> Hypochondria
hypochondria uuid = Hypochondria $ baseAttrs uuid "01100"

instance (ActionRunner env investigator) => HasActions env investigator Hypochondria where
  getActions i NonFast (Hypochondria Attrs {..}) =
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

instance (TreacheryRunner env) => RunMessage env Hypochondria where
  runMessage msg t@(Hypochondria attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage $ AttachTreacheryToInvestigator tid iid
      Hypochondria <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    After (InvestigatorTakeDamage iid _ _)
      | Just iid == treacheryAttachedInvestigator -> t <$ unshiftMessage
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 0 1)
    UseCardAbility _ _ (TreacherySource tid) 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Hypochondria <$> runMessage msg attrs
