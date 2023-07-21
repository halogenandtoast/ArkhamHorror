module Arkham.Treachery.Cards.CursedSwamp (
  CursedSwamp (..),
  cursedSwamp,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype CursedSwamp = CursedSwamp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedSwamp :: TreacheryCard CursedSwamp
cursedSwamp = treachery CursedSwamp Cards.cursedSwamp

instance HasModifiersFor CursedSwamp where
  getModifiersFor (InvestigatorTarget iid) (CursedSwamp attrs) = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource _ _ source _) | isSource attrs source -> do
        isBayou <-
          selectAny $
            LocationWithTrait Bayou
              <> LocationWithInvestigator
                (InvestigatorWithId iid)
        pure $ toModifiers attrs [CannotCommitCards AnyCard | isBayou]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage CursedSwamp where
  runMessage msg t@(CursedSwamp attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source
      | isSource attrs source ->
          t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget {} _ n
      | tid == treacheryId ->
          t
            <$ push
              ( InvestigatorAssignDamage
                  iid
                  (TreacherySource treacheryId)
                  DamageAny
                  n
                  0
              )
    _ -> CursedSwamp <$> runMessage msg attrs
