module Arkham.Types.Treachery.Cards.Hypochondria
  ( Hypochondria(..)
  , hypochondria
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

newtype Hypochondria = Hypochondria TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypochondria :: TreacheryCard Hypochondria
hypochondria = treachery Hypochondria Cards.hypochondria

instance HasModifiersFor env Hypochondria where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Hypochondria where
  getActions iid NonFast (Hypochondria a) =
    withTreacheryInvestigator a $ \tormented -> do
      treacheryLocation <- getId tormented
      investigatorLocationId <- getId @LocationId iid
      pure
        [ UseAbility
            iid
            (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 2))
        | treacheryLocation == investigatorLocationId
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Hypochondria where
  runMessage msg t@(Hypochondria attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    After (InvestigatorTakeDamage iid _ n _)
      | treacheryOnInvestigator iid attrs && n > 0
      -> t <$ push
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 0 1)
    UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ push (Discard (TreacheryTarget treacheryId))
    _ -> Hypochondria <$> runMessage msg attrs
