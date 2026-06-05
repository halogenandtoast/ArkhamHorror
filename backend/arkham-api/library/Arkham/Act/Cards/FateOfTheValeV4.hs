module Arkham.Act.Cards.FateOfTheValeV4 (fateOfTheValeV4) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype FateOfTheValeV4 = FateOfTheValeV4 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV4 :: ActCard FateOfTheValeV4
fateOfTheValeV4 = act (3, A) FateOfTheValeV4 Cards.fateOfTheValeV4 Nothing

fateOfTheValeV4ResignKeys :: [ScenarioLogKey]
fateOfTheValeV4ResignKeys =
  [ TheInvestigatorsFoundGas
  , TheInvestigatorsFoundTheosTruck
  , TheRoadIsClear
  , TheSurveyNotesWereRecovered
  , BertieIsFleeing
  , TheSamplesWereFound
  ]

instance HasAbilities FateOfTheValeV4 where
  getAbilities (FateOfTheValeV4 a) =
    extend
      a
      [ restricted a 1 (RememberedAtLeast (Static 5) fateOfTheValeV4ResignKeys)
          $ ActionAbility #resign Nothing (ActionCost 1)
      , onlyOnce $ restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]

instance RunMessage FateOfTheValeV4 where
  runMessage msg a@(FateOfTheValeV4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs (attrs.ability 2)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R5
      pure a
    _ -> FateOfTheValeV4 <$> liftRunMessage msg attrs
