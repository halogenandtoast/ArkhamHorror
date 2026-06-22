module Arkham.Act.Cards.CaptureTheConspirators (captureTheConspirators) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ByTheBook.Helpers

newtype CaptureTheConspirators = CaptureTheConspirators ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captureTheConspirators :: ActCard CaptureTheConspirators
captureTheConspirators = act (1, A) CaptureTheConspirators Cards.captureTheConspirators Nothing

instance HasAbilities CaptureTheConspirators where
  getAbilities = actAbilities \a ->
    scenarioI18n
      [ tooltip "captureTheConspirators.flipConspirator"
          $ restricted a 1 (youExist $ at_ (LocationWithCardsUnderneath AnyCards))
          $ freeTrigger (GroupClueCost (PerPlayer 1) Anywhere)
      , tooltip "captureTheConspirators.exhaustCultist"
          $ restricted a 2 (youExist rolandBanks <> exists (#cultist <> EnemyIsEngagedWith You <> #ready))
          $ FastAbility (ResourceCost 1)
      , mkAbility a 3 (Objective $ forced AnyWindow)
          `withCriteria` InVictoryDisplay (#cultist <> #enemy) (atLeast 10)
      ]

instance RunMessage CaptureTheConspirators where
  runMessage msg a@(CaptureTheConspirators attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \loc -> do
        cards <- fieldMap LocationCardsUnderneath (take 1) loc
        for_ cards \card -> do
          faceUp <- setFacedown False card
          obtainCard faceUp
          createEnemy_ faceUp iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ #cultist <> enemyEngagedWith iid <> #ready
      chooseTargetM iid enemies exhaustThis
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> CaptureTheConspirators <$> liftRunMessage msg attrs
