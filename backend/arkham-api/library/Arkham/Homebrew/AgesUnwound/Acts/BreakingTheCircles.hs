module Arkham.Homebrew.AgesUnwound.Acts.BreakingTheCircles (breakingTheCircles) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype BreakingTheCircles = BreakingTheCircles ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingTheCircles :: ActCard BreakingTheCircles
breakingTheCircles = act (4, A) BreakingTheCircles Cards.breakingTheCircles Nothing

instance HasAbilities BreakingTheCircles where
  getAbilities (BreakingTheCircles a) =
    [ onlyOnce
        $ restricted a 1 (LocationCount 2 $ "Ritual Circle" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage BreakingTheCircles where
  runMessage msg a@(BreakingTheCircles attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> BreakingTheCircles <$> liftRunMessage msg attrs
