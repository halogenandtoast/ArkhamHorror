{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Psychosis where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype Psychosis = Psychosis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

psychosis :: TreacheryId -> Maybe InvestigatorId -> Psychosis
psychosis uuid iid = Psychosis $ weaknessAttrs uuid iid "01099"

instance (ActionRunner env investigator) => HasActions env investigator Psychosis where
  getActions i NonFast (Psychosis Attrs {..}) =
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

instance (TreacheryRunner env) => RunMessage env Psychosis where
  runMessage msg t@(Psychosis attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage $ AttachTreachery tid (InvestigatorTarget iid)
      Psychosis <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    After (InvestigatorTakeDamage iid _ _ n)
      | Just iid == treacheryAttachedInvestigator && n > 0 -> t <$ unshiftMessage
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Psychosis <$> runMessage msg attrs
