module Arkham.Act.Cards.TheExit (theExit) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Casino))

newtype TheExit = TheExit ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theExit :: ActCard TheExit
theExit = act (3, A) TheExit Cards.theExit Nothing

instance HasModifiersFor TheExit where
  getModifiersFor (TheExit a) = do
    modifySelect
      a
      (EnemyWithTrait Casino)
      [ RemoveKeyword Keyword.Aloof
      , LosePatrol
      , AddKeyword Keyword.Hunter
      , ForcePrey (Prey $ InvestigatorWithScarletKey $ scarletKeyIs Keys.theWellspringOfFortune)
      ]

instance HasAbilities TheExit where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      1
      ( exists
          $ ResignedInvestigator
          <> InvestigatorWithScarletKey (scarletKeyIs Keys.theWellspringOfFortune)
      )
      $ Objective
      $ forced AnyWindow

instance RunMessage TheExit where
  runMessage msg a@(TheExit attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (not_ ResignedInvestigator) \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
        
      push R1
      pure a
    _ -> TheExit <$> liftRunMessage msg attrs
