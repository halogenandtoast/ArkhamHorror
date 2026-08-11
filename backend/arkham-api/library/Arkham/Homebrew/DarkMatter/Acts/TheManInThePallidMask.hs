module Arkham.Homebrew.DarkMatter.Acts.TheManInThePallidMask (theManInThePallidMask) where

import Arkham.Ability
import Arkham.Act.Import.Lifted hiding (DiscoverClues)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype TheManInThePallidMask = TheManInThePallidMask ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theManInThePallidMask :: ActCard TheManInThePallidMask
theManInThePallidMask = act (2, A) TheManInThePallidMask Cards.theManInThePallidMask Nothing

{- | "[reaction] After you successfully investigate or discover clues at The
Stranger's location: Deal 2 damage to it. (Limit once per turn.)" /
"Objective - After The Stranger is defeated, advance."
-}
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
    , restricted a 2 (not_ $ exists $ enemyIs Enemies.theStranger) $ Objective $ forced AnyWindow
    ]

instance RunMessage TheManInThePallidMask where
  runMessage msg a@(TheManInThePallidMask attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (enemyIs Enemies.theStranger)
        >>= traverse_ (nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheManInThePallidMask <$> liftRunMessage msg attrs
