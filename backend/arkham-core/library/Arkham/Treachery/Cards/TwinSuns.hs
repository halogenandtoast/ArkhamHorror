module Arkham.Treachery.Cards.TwinSuns (twinSuns, TwinSuns (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TwinSuns = TwinSuns TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twinSuns :: TreacheryCard TwinSuns
twinSuns = treachery TwinSuns Cards.twinSuns

instance RunMessage TwinSuns where
  runMessage msg t@(TwinSuns attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      agenda <- selectJust AnyAgenda
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Remove 1 doom from the current agenda" [RemoveDoom (toSource attrs) (toTarget agenda) 1]
          , Label "Take 1 horror for each point you failed by" [assignHorror iid attrs n]
          ]
      pure t
    _ -> TwinSuns <$> runMessage msg attrs
