module Arkham.Treachery.Cards.CursedSwamp
  ( CursedSwamp(..)
  , cursedSwamp
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Attrs
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype CursedSwamp = CursedSwamp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedSwamp :: TreacheryCard CursedSwamp
cursedSwamp = treachery CursedSwamp Cards.cursedSwamp

instance
  ( HasId LocationId env InvestigatorId
  , HasSet Trait env LocationId
  )
  => HasModifiersFor env CursedSwamp where
  getModifiersFor (SkillTestSource _ _ source _) (InvestigatorTarget iid) (CursedSwamp attrs)
    | isSource attrs source
    = do
      locationId <- getId @LocationId iid
      isBayou <- member Bayou <$> getSet locationId
      pure $ toModifiers attrs [ CannotCommitCards AnyCard | isBayou ]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage env CursedSwamp where
  runMessage msg t@(CursedSwamp attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId
      -> t
        <$ push
             (InvestigatorAssignDamage
               iid
               (TreacherySource treacheryId)
               DamageAny
               n
               0
             )
    _ -> CursedSwamp <$> runMessage msg attrs
