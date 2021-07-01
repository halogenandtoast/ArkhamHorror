module Arkham.Types.Treachery.Cards.Chronophobia
  ( chronophobia
  , Chronophobia(..)
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

newtype Chronophobia = Chronophobia TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chronophobia :: TreacheryCard Chronophobia
chronophobia = treachery Chronophobia Cards.chronophobia

instance HasModifiersFor env Chronophobia where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Chronophobia where
  getActions iid NonFast (Chronophobia a) =
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

instance (TreacheryRunner env) => RunMessage env Chronophobia where
  runMessage msg t@(Chronophobia attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    EndTurn iid | InvestigatorTarget iid `elem` treacheryAttachedTarget ->
      t <$ unshiftMessage (InvestigatorDirectDamage iid (toSource attrs) 0 1)
    UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> Chronophobia <$> runMessage msg attrs
