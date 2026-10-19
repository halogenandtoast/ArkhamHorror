module Arkham.Homebrew.AgesUnwound.Acts.BreakingTheCircle (breakingTheCircle) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype BreakingTheCircle = BreakingTheCircle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingTheCircle :: ActCard BreakingTheCircle
breakingTheCircle = act (3, A) BreakingTheCircle Cards.breakingTheCircle Nothing

instance HasAbilities BreakingTheCircle where
  getAbilities (BreakingTheCircle a) =
    [ onlyOnce
        $ restricted a 1 (exists $ "Ritual Circle" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage BreakingTheCircle where
  runMessage msg a@(BreakingTheCircle attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record TheInvestigatorsBrokeTheFirstCircle
      recordTheTimeFor TheInvestigatorsBrokeTheFirstCircle
      push R3
      pure a
    _ -> BreakingTheCircle <$> liftRunMessage msg attrs
