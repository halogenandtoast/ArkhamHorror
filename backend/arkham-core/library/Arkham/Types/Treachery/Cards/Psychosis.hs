module Arkham.Types.Treachery.Cards.Psychosis
  ( Psychosis(..)
  , psychosis
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Psychosis = Psychosis TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychosis :: TreacheryId -> Maybe InvestigatorId -> Psychosis
psychosis uuid iid = Psychosis $ weaknessAttrs uuid iid "01099"

instance HasModifiersFor env Psychosis where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Psychosis where
  getActions iid NonFast (Psychosis a@TreacheryAttrs {..}) =
    withTreacheryInvestigator a $ \tormented -> do
      investigatorLocationId <- getId @LocationId iid
      treacheryLocation <- getId tormented
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 2))
        | treacheryLocation == investigatorLocationId
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Psychosis where
  runMessage msg t@(Psychosis attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    After (InvestigatorTakeDamage iid _ _ n)
      | treacheryOnInvestigator iid attrs && n > 0 -> t <$ unshiftMessage
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Psychosis <$> runMessage msg attrs
