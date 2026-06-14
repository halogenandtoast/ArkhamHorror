module Arkham.Act.Cards.ScouringTheSpires (scouringTheSpires) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype ScouringTheSpires = ScouringTheSpires ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scouringTheSpires :: ActCard ScouringTheSpires
scouringTheSpires = act (1, A) ScouringTheSpires Cards.scouringTheSpires Nothing

instance HasAbilities ScouringTheSpires where
  getAbilities (ScouringTheSpires attrs) =
    extend
      attrs
      [ -- [action] Spend X clues: Reveal X cards from the bottom of the Summit deck...
        mkAbility attrs 1 $ actionAbilityWithCost ClueCostX
      , -- Objective - If each surviving investigator is at Central Spire, advance.
        restricted
          attrs
          2
          (notExists $ UneliminatedInvestigator <> not_ (InvestigatorAt $ locationIs Locations.centralSpire))
          $ Objective
          $ forced AnyWindow
      ]

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance RunMessage ScouringTheSpires where
  runMessage msg a@(ScouringTheSpires attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (getClueCount -> _x) -> do
      -- TODO: Reveal _x cards from the BOTTOM of the Summit deck. You may put 1
      -- revealed location into play in an adjacent open sky and move to it.
      -- (Place that open sky card and each other revealed card on top of the
      -- Summit deck in any order.) The Summit deck, "open sky" placeholder
      -- cards, and sliding/adjacency placement have no engine support yet, so
      -- the reveal/place/move portion is left unimplemented.
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Floating Skyline (side B):
      -- TODO: Remove R'lyeh Streets from the game, ignoring its text box.
      -- Shuffle each open sky and Summit location in play except for Central
      -- Spire into the Summit deck. Each card and token at those locations is
      -- discarded. Put 5 of the set-aside open sky cards into play per the Act 2
      -- placement diagram, fill rows/columns from the bottom of the Summit deck,
      -- then shuffle the set-aside Floating Spire and Aerial Waterfall locations
      -- into the top 3 cards of the Summit deck. Mirrors SearchingTheSpires's
      -- advance minus the Inescapable spawn; requires Summit-deck/open-sky/
      -- sliding-location infra that does not exist yet.
      pure a
    _ -> ScouringTheSpires <$> liftRunMessage msg attrs
