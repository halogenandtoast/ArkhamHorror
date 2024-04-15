module Arkham.Treachery.Cards.MarkedByTheSign (
  markedByTheSign,
  MarkedByTheSign (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MarkedByTheSign = MarkedByTheSign TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedByTheSign :: TreacheryCard MarkedByTheSign
markedByTheSign = treachery MarkedByTheSign Cards.markedByTheSign

instance RunMessage MarkedByTheSign where
  runMessage msg t@(MarkedByTheSign attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      theManInThePallidMaskIsInPlay <-
        selectAny
          $ enemyIs Cards.theManInThePallidMask
      let difficulty = if theManInThePallidMaskIsInPlay then 4 else 2
      t <$ push (RevelationSkillTest iid source SkillWillpower (SkillTestDifficulty $ Fixed difficulty))
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ -> do
      theManInThePallidMaskIsInPlay <-
        selectAny
          $ enemyIs Cards.theManInThePallidMask
      t
        <$ if theManInThePallidMaskIsInPlay
          then push (InvestigatorDirectDamage iid source 0 2)
          else push (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> MarkedByTheSign <$> runMessage msg attrs
