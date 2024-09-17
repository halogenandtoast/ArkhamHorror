module Arkham.Enemy.Cards.OtheraGilmanProprietessOfTheHotel
  ( otheraGilmanProprietessOfTheHotel
  , OtheraGilmanProprietessOfTheHotel(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype OtheraGilmanProprietessOfTheHotel = OtheraGilmanProprietessOfTheHotel EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

otheraGilmanProprietessOfTheHotel :: EnemyCard OtheraGilmanProprietessOfTheHotel
otheraGilmanProprietessOfTheHotel = enemy OtheraGilmanProprietessOfTheHotel Cards.otheraGilmanProprietessOfTheHotel (3, Static 5, 3) (0, 2)

instance RunMessage OtheraGilmanProprietessOfTheHotel where
  runMessage msg (OtheraGilmanProprietessOfTheHotel attrs) =
    OtheraGilmanProprietessOfTheHotel <$> runMessage msg attrs
