module Arkham.Act.Cards.TheLostExpedition (TheLostExpedition (..), theLostExpedition) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.IceAndDeath.Helpers
import Data.Map.Strict qualified as Map

newtype TheLostExpedition = TheLostExpedition ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLostExpedition :: ActCard TheLostExpedition
theLostExpedition = act (1, A) TheLostExpedition Cards.theLostExpedition Nothing

instance HasAbilities TheLostExpedition where
  getAbilities (TheLostExpedition a) =
    extend
      a
      [ mkAbility a 1
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (YourLocation <> LocationWithCardsUnderneath AnyCards)
      , restricted a 2 atCamp $ ActionAbility [#resign] (ActionCost 1)
      , restricted a 3 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]
   where
    atCamp = oneOf $ map toCriteria (Map.toList camps)
    toCriteria (site, campRecord) = HasRecord campRecord <> youExist (at_ $ LocationIs site)

instance RunMessage TheLostExpedition where
  runMessage msg a@(TheLostExpedition attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        cards <- field LocationCardsUnderneath lid
        for_ cards (drawCard iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resign iid
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    _ -> TheLostExpedition <$> liftRunMessage msg attrs
