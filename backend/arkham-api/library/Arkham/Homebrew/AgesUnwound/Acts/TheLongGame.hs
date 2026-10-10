module Arkham.Homebrew.AgesUnwound.Acts.TheLongGame (theLongGame) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (putTaskIntoPlayWithRevelation)
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern TaskDeck)
import Arkham.Matcher

newtype TheLongGame = TheLongGame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /The Long Game/ (@:ages-unwound:110@), the scenario's only act. It never
advances on progress -- the year ends when everyone has resigned, or when Summer
runs out and sends the table to Resolution 1 directly.
-}
theLongGame :: ActCard TheLongGame
theLongGame = act (1, A) TheLongGame Cards.theLongGame Nothing

{- | "[free] The investigators spend 1[per_investigator] clues, as a group: Put
the top card of the Task deck into play next to the act deck, resolving its
revelation effect. (Group limit once per round.)" / "__Objective__ - Complete as
many [[Task]] treacheries as possible. If each undefeated investigator has
resigned, advance."
-}
instance HasAbilities TheLongGame where
  getAbilities (TheLongGame a) =
    extend
      a
      [ campaignI18n
          $ withI18nTooltip "theLongGame.drawTask"
          $ groupLimit PerRound
          $ restricted a 1 (ScenarioDeckWithCard TaskDeck)
          $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
      , onlyOnce
          $ restricted a 2 (notExists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage TheLongGame where
  runMessage msg a@(TheLongGame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      {- The Task is already a real card sitting in the scenario deck, so it is
      struck from the deck and handed over as-is; minting a fresh copy would
      leave the original behind. -}
      getScenarioDeck TaskDeck >>= \case
        [] -> pure ()
        card : _ -> do
          push $ RemoveCardFromScenarioDeck TaskDeck card
          putTaskIntoPlayWithRevelation iid card
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    -- "__The Beginning of the End__ - ->R1."
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheLongGame <$> liftRunMessage msg attrs
