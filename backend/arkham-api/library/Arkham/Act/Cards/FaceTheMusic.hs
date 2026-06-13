module Arkham.Act.Cards.FaceTheMusic (faceTheMusic) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Criminal, Elite))

newtype FaceTheMusic = FaceTheMusic ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

faceTheMusic :: ActCard FaceTheMusic
faceTheMusic = act (3, A) FaceTheMusic Cards.faceTheMusic Nothing

instance HasModifiersFor FaceTheMusic where
  getModifiersFor (FaceTheMusic a) = do
    criminalsWithClues <- select $ EnemyWithTrait Criminal <> EnemyWithClues (atLeast 1)
    modifyEach a criminalsWithClues [AddKeyword Keyword.Aloof]

-- TODO: ability 1 - "Remove 1 clue from a Criminal enemy at your location:
-- Parley. That enemy deals its damage to another enemy at your location."
instance HasAbilities FaceTheMusic where
  getAbilities (FaceTheMusic a) | onSide A a =
    [ restricted a 1 (InVictoryDisplay (CardWithTrait Elite) (atLeast 1))
        $ Objective
        $ forced
        $ RoundEnds #when
    ]
  getAbilities _ = []

instance RunMessage FaceTheMusic where
  runMessage msg a@(FaceTheMusic attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> FaceTheMusic <$> liftRunMessage msg attrs
