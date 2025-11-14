module Arkham.Act.Cards.TheTake (theTake) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Trait (Trait (Game))

newtype TheTake = TheTake ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTake :: ActCard TheTake
theTake = act (2, A) TheTake Cards.theTake Nothing

instance HasAbilities TheTake where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ freeReaction
        $ ActivateAbility #after You
        $ AbilityOnLocation (LocationWithTrait Game)
        <> AbilityIsActionAbility
    , restricted a 2 (exists $ InvestigatorWithScarletKey $ scarletKeyIs Keys.theWellspringOfFortune)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheTake where
  runMessage msg a@(TheTake attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (scarletKeyIs Keys.theWellspringOfFortune) \wellspring -> do
        gotResources <- matches iid (InvestigatorWithModifier (ScenarioModifier "gotResources"))
        removeTokens (attrs.ability 1) wellspring #clue $ if gotResources then 2 else 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      mabbaran <- selectOne $ InPlayEnemy $ enemyIs Enemies.abarranArrigorriagakoaAbarranUnleashed
      case mabbaran of
        Just abbaran -> enemyMoveTo attrs abbaran =<< getJustLocationByName "Relic Room"
        Nothing -> eachInvestigator \iid -> do
          sid <- getRandom
          beginSkillTest sid iid attrs iid #agility (Fixed 3)
      advanceActDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure a
    _ -> TheTake <$> liftRunMessage msg attrs
