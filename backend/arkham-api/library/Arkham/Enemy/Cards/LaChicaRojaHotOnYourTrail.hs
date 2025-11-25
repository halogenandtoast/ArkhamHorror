module Arkham.Enemy.Cards.LaChicaRojaHotOnYourTrail (laChicaRojaHotOnYourTrail) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype LaChicaRojaHotOnYourTrail = LaChicaRojaHotOnYourTrail EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laChicaRojaHotOnYourTrail :: EnemyCard LaChicaRojaHotOnYourTrail
laChicaRojaHotOnYourTrail =
  enemy LaChicaRojaHotOnYourTrail Cards.laChicaRojaHotOnYourTrail (3, Static 3, 5) (1, 1)
    & setSpawnAt (FarthestLocationFromYou Anywhere)

instance HasAbilities LaChicaRojaHotOnYourTrail where
  getAbilities (LaChicaRojaHotOnYourTrail a) =
    extend
      a
      [ restricted a 1 (thisExists a $ UnengagedEnemy <> EnemyWithAnyScarletKey)
          $ forced
          $ PhaseEnds #when #enemy
      , restricted a 2 (thisExists a $ EnemyWithToken #resource)
          $ freeReaction
          $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage LaChicaRojaHotOnYourTrail where
  runMessage msg e@(LaChicaRojaHotOnYourTrail attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach InvestigatorWithAnyResources \iid -> moveTokens (attrs.ability 1) iid attrs #resource 1
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      lead <- getLead
      chooseOneAtATimeM lead $ targets skeys shift
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <- getInvestigators
      repeated (attrs.token #resource) $ chooseOrRunOneM iid $ for_ iids \iid' ->
        resourceLabeled iid' $ gainResources iid' attrs 1
      pure e
    _ -> LaChicaRojaHotOnYourTrail <$> liftRunMessage msg attrs
