module Arkham.Act.Cards.ToTheAncientDome (toTheAncientDome) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Scenarios.ObsidianCanyons.Helpers (scenarioI18n)

newtype ToTheAncientDome = ToTheAncientDome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toTheAncientDome :: ActCard ToTheAncientDome
toTheAncientDome = act (2, A) ToTheAncientDome Cards.toTheAncientDome Nothing

instance HasAbilities ToTheAncientDome where
  getAbilities (ToTheAncientDome a) =
    extend
      a
      [ restricted a 1 (DuringTurn You) actionAbility
      , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]

instance RunMessage ToTheAncientDome where
  runMessage msg a@(ToTheAncientDome attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "[action] Spend X clues" — the player chooses X (bounded by the clues
      -- they can spend) and we deduct them up front.
      n <- getSpendableClueCount iid
      when (n > 0) $ scenarioI18n $ chooseAmount' iid "cluesToSpend" "$clues" 0 n attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      spendClues iid n
      -- TODO(Summit deck / open sky): "Reveal X cards from the bottom of the
      -- Summit deck. You may put 1 revealed location into play in an adjacent
      -- open sky and move to it. (Place that open sky card and each other
      -- revealed card on top of the Summit deck in any order.)"
      -- The Summit deck, "open sky" placeholder cards, and sliding/placing
      -- locations into open sky have no engine support yet, so the reveal/place/
      -- move portion is left unimplemented. Once those primitives exist, reveal
      -- the bottom n cards here, let the player put a revealed location into an
      -- adjacent open sky and moveTo it, then return the rest to the top.
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> ToTheAncientDome <$> liftRunMessage msg attrs
