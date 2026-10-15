module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Enemies.LuminousPhantasm (luminousPhantasm) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers (ruinedLocation, scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (ScenarioDiscard))
import Arkham.Trait (Trait (Curse))

newtype LuminousPhantasm = LuminousPhantasm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousPhantasm :: EnemyCard LuminousPhantasm
luminousPhantasm =
  enemy LuminousPhantasm Cards.luminousPhantasm
    & setSpawnAtFirst [EmptyLocation <> ruinedLocation, EmptyLocation]

instance HasAbilities LuminousPhantasm where
  getAbilities (LuminousPhantasm a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage LuminousPhantasm where
  runMessage msg e@(LuminousPhantasm attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mCurse <- find (`cardMatch` (#treachery <> CardWithTrait Curse)) <$> scenarioField ScenarioDiscard
      chooseOneM iid $ scenarioI18n $ scope "luminousPhantasm" do
        unscoped $ labeled "takeTwoHorror" $ assignHorror iid (attrs.ability 1) 2
        for_ mCurse
          $ labeled "drawTopmostCurse"
          . push
          . DrewTreachery iid (Just Deck.EncounterDiscard)
          . toCard
      pure e
    _ -> LuminousPhantasm <$> liftRunMessage msg attrs
