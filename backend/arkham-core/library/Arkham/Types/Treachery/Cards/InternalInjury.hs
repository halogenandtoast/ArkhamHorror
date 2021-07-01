module Arkham.Types.Treachery.Cards.InternalInjury
  ( internalInjury
  , InternalInjury(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.Window

newtype InternalInjury = InternalInjury TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

internalInjury :: TreacheryCard InternalInjury
internalInjury = treachery InternalInjury Cards.internalInjury

instance HasModifiersFor env InternalInjury where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env InternalInjury where
  getActions iid NonFast (InternalInjury a) =
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

instance (TreacheryRunner env) => RunMessage env InternalInjury where
  runMessage msg t@(InternalInjury attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    EndTurn iid | InvestigatorTarget iid `elem` treacheryAttachedTarget ->
      t <$ unshiftMessage (InvestigatorDirectDamage iid (toSource attrs) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> InternalInjury <$> runMessage msg attrs
