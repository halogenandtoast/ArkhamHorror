module Arkham.Homebrew.CircusExMortis.Acts.DeeperProfanities (deeperProfanities) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype DeeperProfanities = DeeperProfanities ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deeperProfanities :: ActCard DeeperProfanities
deeperProfanities = act (2, A) DeeperProfanities Cards.deeperProfanities Nothing

instance HasAbilities DeeperProfanities where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1 $ Objective $ forced $ Enters #after Anyone (locationIs Locations.savageAltar)

instance RunMessage DeeperProfanities where
  runMessage msg a@(DeeperProfanities attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      bothRevealed <-
        (== 2)
          <$> selectCount
            (RevealedLocation <> mapOneOf locationIs [Locations.manorCellars, Locations.hiddenDungeon])
      createSetAsideEnemyWith_
        Enemies.goatspawnCorruptor
        Locations.savageAltar
        (if bothRevealed then createExhausted else id)
      advanceActDeck attrs
      pure a
    _ -> DeeperProfanities <$> liftRunMessage msg attrs
