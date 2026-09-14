module Arkham.Homebrew.CircusExMortis.Enemies.TwistedSatyr (twistedSatyr) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher

newtype TwistedSatyr = TwistedSatyr EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedSatyr :: EnemyCard TwistedSatyr
twistedSatyr = enemy TwistedSatyr Cards.twistedSatyr

instance HasAbilities TwistedSatyr where
  getAbilities (TwistedSatyr a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage TwistedSatyr where
  runMessage msg e@(TwistedSatyr attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (chaosToken_ (ChaosTokenFaceIs MoonToken)) >>= \case
        Just token -> sealChaosToken iid iid token
        Nothing -> assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> TwistedSatyr <$> liftRunMessage msg attrs
