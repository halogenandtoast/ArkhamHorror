module Arkham.Types.Treachery.Cards.Atychiphobia
  ( atychiphobia
  , Atychiphobia(..)
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

newtype Atychiphobia = Atychiphobia TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atychiphobia :: TreacheryCard Atychiphobia
atychiphobia = treachery Atychiphobia Cards.atychiphobia

instance HasModifiersFor env Atychiphobia

instance ActionRunner env => HasActions env Atychiphobia where
  getActions iid NonFast (Atychiphobia a) =
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

instance (TreacheryRunner env) => RunMessage env Atychiphobia where
  runMessage msg t@(Atychiphobia attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    FailedSkillTest iid _ _ SkillTestInitiatorTarget{} _ _
      | treacheryOnInvestigator iid attrs
      -> t
        <$ push
             (InvestigatorAssignDamage
               iid
               (TreacherySource treacheryId)
               DamageAny
               0
               1
             )
    UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ push (Discard (TreacheryTarget treacheryId))
    _ -> Atychiphobia <$> runMessage msg attrs
