module Arkham.Homebrew.CircusExMortis.Enemies.TwistedSatyr (twistedSatyr) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype TwistedSatyr = TwistedSatyr EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedSatyr :: EnemyCard TwistedSatyr
twistedSatyr = enemy TwistedSatyr Cards.twistedSatyr

instance HasModifiersFor TwistedSatyr where
  getModifiersFor (TwistedSatyr a) =
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate]

instance HasAbilities TwistedSatyr where
  getAbilities (TwistedSatyr a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage TwistedSatyr where
  runMessage msg e@(TwistedSatyr attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (chaosToken_ #moon) >>= \case
        Just token -> sealChaosToken iid iid token
        Nothing -> assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> TwistedSatyr <$> liftRunMessage msg attrs
