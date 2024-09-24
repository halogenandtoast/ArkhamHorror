module Arkham.Enemy.Cards.OtheraGilmanProprietessOfTheHotel (
  otheraGilmanProprietessOfTheHotel,
  OtheraGilmanProprietessOfTheHotel (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype OtheraGilmanProprietessOfTheHotel = OtheraGilmanProprietessOfTheHotel EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otheraGilmanProprietessOfTheHotel :: EnemyCard OtheraGilmanProprietessOfTheHotel
otheraGilmanProprietessOfTheHotel =
  enemy
    OtheraGilmanProprietessOfTheHotel
    Cards.otheraGilmanProprietessOfTheHotel
    (3, Static 5, 3)
    (0, 2)

instance HasAbilities OtheraGilmanProprietessOfTheHotel where
  getAbilities (OtheraGilmanProprietessOfTheHotel a) =
    extend
      a
      [restrictedAbility a 1 OnSameLocation $ parleyAction $ ResourceCost 3]

instance RunMessage OtheraGilmanProprietessOfTheHotel where
  runMessage msg e@(OtheraGilmanProprietessOfTheHotel attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeClues attrs attrs =<< perPlayer 1
      place attrs =<< selectJust (LocationWithTitle "Gilman House")
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) attrs iid #clue 1
      doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      when (attrs.token #clue == 0) $ addToVictory attrs
      pure e
    _ -> OtheraGilmanProprietessOfTheHotel <$> liftRunMessage msg attrs
