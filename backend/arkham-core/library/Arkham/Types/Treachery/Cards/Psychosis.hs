module Arkham.Types.Treachery.Cards.Psychosis
  ( Psychosis(..)
  , psychosis
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

newtype Psychosis = Psychosis TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychosis :: TreacheryCard Psychosis
psychosis = treachery Psychosis Cards.psychosis

instance HasModifiersFor env Psychosis

instance ActionRunner env => HasActions env Psychosis where
  getActions iid NonFast (Psychosis a) =
    withTreacheryInvestigator a $ \tormented -> do
      investigatorLocationId <- getId @LocationId iid
      treacheryLocation <- getId tormented
      pure
        [ UseAbility
            iid
            (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 2))
        | treacheryLocation == investigatorLocationId
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Psychosis where
  runMessage msg t@(Psychosis attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    After (InvestigatorTakeDamage iid _ _ n)
      | treacheryOnInvestigator iid attrs && n > 0
      -> t <$ push
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ push (Discard (TreacheryTarget treacheryId))
    _ -> Psychosis <$> runMessage msg attrs
