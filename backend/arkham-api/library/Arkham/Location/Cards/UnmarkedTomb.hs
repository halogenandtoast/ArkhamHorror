module Arkham.Location.Cards.UnmarkedTomb (unmarkedTomb, UnmarkedTomb (..)) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype UnmarkedTomb = UnmarkedTomb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unmarkedTomb :: LocationCard UnmarkedTomb
unmarkedTomb = location UnmarkedTomb Cards.unmarkedTomb 3 (PerPlayer 1)

instance HasAbilities UnmarkedTomb where
  getAbilities (UnmarkedTomb x) =
    extendRevealed
      x
      [ restrictedAbility
          x
          1
          (EachUndefeatedInvestigator (investigatorAt x) <> Remembered RecoveredAStrangeKey)
          $ Objective
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage UnmarkedTomb where
  runMessage msg l@(UnmarkedTomb attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource attrs) AdvancedWithOther
      pure l
    _ -> UnmarkedTomb <$> runMessage msg attrs
