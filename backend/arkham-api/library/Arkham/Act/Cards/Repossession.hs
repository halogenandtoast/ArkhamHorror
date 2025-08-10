module Arkham.Act.Cards.Repossession (repossession) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Projection

newtype Repossession = Repossession ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

repossession :: ActCard Repossession
repossession = act (3, A) Repossession Cards.repossession Nothing

instance HasModifiersFor Repossession where
  getModifiersFor (Repossession a) = do
    modifySelect a (enemyIs Enemies.yithianObserver) [AddKeyword Hunter]

instance HasAbilities Repossession where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ ActionAbility [#draw] $ ClueCost (Static 1) <> ActionCost 1
    , restricted a 2 (EachUndefeatedInvestigator $ HandWith $ LengthIs $ atLeast 10)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage Repossession where
  runMessage msg a@(Repossession attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handSize <- field InvestigatorHandSize iid
      numberOfCardsInHand <- fieldMap InvestigatorHand length iid
      let drawCount = if numberOfCardsInHand > handSize then 3 else 2
      drawCards iid (attrs.ability 1) drawCount
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> Repossession <$> liftRunMessage msg attrs
