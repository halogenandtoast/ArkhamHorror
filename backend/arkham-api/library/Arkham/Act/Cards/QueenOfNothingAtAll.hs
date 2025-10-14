module Arkham.Act.Cards.QueenOfNothingAtAll (queenOfNothingAtAll) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype QueenOfNothingAtAll = QueenOfNothingAtAll ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

queenOfNothingAtAll :: ActCard QueenOfNothingAtAll
queenOfNothingAtAll = act (3, A) QueenOfNothingAtAll Cards.queenOfNothingAtAll Nothing

instance HasAbilities QueenOfNothingAtAll where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      1
      ( not_
          $ exists
          $ mapOneOf
            enemyIs
            [ Enemies.amaranthLurkingCorruption
            , Enemies.amaranthCorruptionRevealed
            , Enemies.razinFarhiReanimatedArtificer
            ]
      )
      $ forced AnyWindow

instance RunMessage QueenOfNothingAtAll where
  runMessage msg a@(QueenOfNothingAtAll attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      knowsRealName <- getHasRecord TheCellKnowsAmaranthsRealName
      push $ if knowsRealName then R3 else R4
      pure a
    _ -> QueenOfNothingAtAll <$> liftRunMessage msg attrs
