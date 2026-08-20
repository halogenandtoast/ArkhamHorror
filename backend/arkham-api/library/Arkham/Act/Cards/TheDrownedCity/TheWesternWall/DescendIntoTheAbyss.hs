module Arkham.Act.Cards.TheDrownedCity.TheWesternWall.DescendIntoTheAbyss (descendIntoTheAbyss) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheDrownedCity.TheWesternWall qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher hiding (DuringTurn)

newtype DescendIntoTheAbyss = DescendIntoTheAbyss ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descendIntoTheAbyss :: ActCard DescendIntoTheAbyss
descendIntoTheAbyss = act (1, A) DescendIntoTheAbyss Cards.descendIntoTheAbyss Nothing

instance HasAbilities DescendIntoTheAbyss where
  getAbilities (DescendIntoTheAbyss a) =
    extend
      a
      [ restricted a 1 (youExist $ at_ FloodedLocation) $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
      , onlyOnce
          $ restricted
            a
            2
            (EachUndefeatedInvestigator (at_ $ location_ "Obsidian Foundations") <> DuringTurn Anyone)
          $ Objective freeTrigger_
      ]

instance RunMessage DescendIntoTheAbyss where
  runMessage msg a@(DescendIntoTheAbyss attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid decreaseFloodLevel
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> DescendIntoTheAbyss <$> liftRunMessage msg attrs
