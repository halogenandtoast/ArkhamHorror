module Arkham.Enemy.Cards.TwistedSatyrCircusExMortis (twistedSatyrCircusExMortis) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype TwistedSatyrCircusExMortis = TwistedSatyrCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedSatyrCircusExMortis :: EnemyCard TwistedSatyrCircusExMortis
twistedSatyrCircusExMortis = enemy TwistedSatyrCircusExMortis Cards.twistedSatyrCircusExMortis

instance HasModifiersFor TwistedSatyrCircusExMortis where
  getModifiersFor (TwistedSatyrCircusExMortis a) =
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate]

instance HasAbilities TwistedSatyrCircusExMortis where
  getAbilities (TwistedSatyrCircusExMortis a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage TwistedSatyrCircusExMortis where
  runMessage msg e@(TwistedSatyrCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (chaosToken_ #moon) >>= \case
        Just token -> sealChaosToken iid iid token
        Nothing -> assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> TwistedSatyrCircusExMortis <$> liftRunMessage msg attrs
