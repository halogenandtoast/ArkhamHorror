module Arkham.Treachery.Cards.Bathophobia (
  bathophobia,
  Bathophobia (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Bathophobia = Bathophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathophobia :: TreacheryCard Bathophobia
bathophobia = treachery Bathophobia Cards.bathophobia

instance RunMessage Bathophobia where
  runMessage msg t@(Bathophobia attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push
        $ revelationSkillTest
          iid
          attrs
          #willpower
          (SumCalculation [Fixed 1, ScenarioCount CurrentDepth])
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ assignHorror iid attrs 2
      pure t
    _ -> Bathophobia <$> runMessage msg attrs
