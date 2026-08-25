module Arkham.Homebrew.DarkMatter.Enemies.Tassilda (tassilda) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (getDoomAmount)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getImpendingDoom)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype Tassilda = Tassilda EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tassilda :: EnemyCard Tassilda
tassilda = enemy Tassilda Cards.tassilda

instance HasModifiersFor Tassilda where
  getModifiersFor (Tassilda a) = do
    doom <- getImpendingDoom
    bonus <- perPlayer doom
    modifySelf a
      $ [AddKeyword Keyword.Massive, AddKeyword Keyword.Retaliate]
      <> [HealthModifier bonus | bonus > 0]

instance HasAbilities Tassilda where
  getAbilities (Tassilda a) =
    extend1 a $ mkAbility a 1 $ forced $ PlacedDoomCounter #after AnySource AnyTarget

instance RunMessage Tassilda where
  runMessage msg e@(Tassilda attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDoomAmount -> n) _ -> do
      locations <-
        select $ oneOf [locationWithEnemy attrs.id, connectedTo (locationWithEnemy attrs.id)]
      for_ locations \lid -> placeTokens (attrs.ability 1) lid Token.Horror n
      pure e
    _ -> Tassilda <$> liftRunMessage msg attrs
