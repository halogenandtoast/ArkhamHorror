module Arkham.Act.Cards.CaptureTheConspirators (captureTheConspirators) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ByTheBook.Helpers
import Arkham.Trait (Trait (Cultist))

newtype CaptureTheConspirators = CaptureTheConspirators ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captureTheConspirators :: ActCard CaptureTheConspirators
captureTheConspirators = act (1, A) CaptureTheConspirators Cards.captureTheConspirators Nothing

instance HasAbilities CaptureTheConspirators where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ You <> at_ (LocationWithCardsUnderneath AnyCards))
        $ freeTrigger (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted
        a
        2
        (youExist rolandBanks <> exists (EnemyWithTrait Cultist <> EnemyIsEngagedWith You <> ReadyEnemy))
        $ FastAbility (ResourceCost 1)
    , mkAbility a 3 (Objective $ forced AnyWindow)
        `withCriteria` InVictoryDisplay (CardWithTrait Cultist <> #enemy) (AtLeast $ Static 10)
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
      enemies <- select $ EnemyWithTrait Cultist <> enemyEngagedWith iid <> ReadyEnemy
      chooseTargetM iid enemies exhaustThis
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      scenarioI18n $ scope "secretsUncovered" $ flavor $ h "title" >> p "body"
      push R1
      pure a
    _ -> CaptureTheConspirators <$> liftRunMessage msg attrs
