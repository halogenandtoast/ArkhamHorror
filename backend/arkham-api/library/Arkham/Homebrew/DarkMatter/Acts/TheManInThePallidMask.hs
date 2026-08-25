module Arkham.Homebrew.DarkMatter.Acts.TheManInThePallidMask (theManInThePallidMask) where

import Arkham.Ability
import Arkham.Act.Import.Lifted hiding (DiscoverClues)
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardsMatching)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (addMemories, scenarioI18n)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheManInThePallidMask = TheManInThePallidMask ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theManInThePallidMask :: ActCard TheManInThePallidMask
theManInThePallidMask = act (2, A) TheManInThePallidMask Cards.theManInThePallidMask Nothing

instance HasAbilities TheManInThePallidMask where
  getAbilities (TheManInThePallidMask a) =
    [ playerLimit PerTurn
        $ restricted a 1 (exists $ enemyIs Enemies.theStranger)
        $ freeReaction
        $ oneOf
          [ SkillTestResult
              #after
              You
              (WhileInvestigating $ LocationWithEnemy $ enemyIs Enemies.theStranger)
              (SuccessResult AnyValue)
          , DiscoverClues #after You (LocationWithEnemy $ enemyIs Enemies.theStranger) AnyValue
          ]
    , onlyOnce
        $ restricted a 2 (not_ $ exists $ enemyIs Enemies.theStranger)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheManInThePallidMask where
  runMessage msg a@(TheManInThePallidMask attrs) = runQueueT $ scenarioI18n "theMachineInYellow" $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (enemyIs Enemies.theStranger)
        >>= traverse_ (nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      copies <- getSetAsideCardsMatching $ cardIs Enemies.yourOtherSelf
      investigators <- getInvestigators
      for_ (zip investigators copies) \(iid, copy) -> createEnemy_ copy iid
      whenM (getHasRecord YouHaveUncoveredTheCultistsInhumanMethods) do
        eachInvestigator \iid -> chooseOneM iid $ scope "theManInThePallidMask" do
          labeled' "gainClueAndHeal" do
            gainClues iid attrs 1
            healDamage iid attrs 2
            healHorror iid attrs 2
          labeled' "addMemory" $ addMemories iid 1
          unscoped $ labeled' "doNothing" nothing
      advanceActDeck attrs
      pure a
    _ -> TheManInThePallidMask <$> liftRunMessage msg attrs
