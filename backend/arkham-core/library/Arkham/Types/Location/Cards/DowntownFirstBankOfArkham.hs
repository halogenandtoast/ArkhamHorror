module Arkham.Types.Location.Cards.DowntownFirstBankOfArkham
  ( DowntownFirstBankOfArkham(..)
  , downtownFirstBankOfArkham
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (downtownFirstBankOfArkham)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham :: LocationCard DowntownFirstBankOfArkham
downtownFirstBankOfArkham = location
  DowntownFirstBankOfArkham
  Cards.downtownFirstBankOfArkham
  3
  (PerPlayer 1)
  Triangle
  [Moon, T]

instance HasAbilities DowntownFirstBankOfArkham where
  getAbilities (DowntownFirstBankOfArkham attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
          attrs
          1
          (Here <> CanGainResources)
          (ActionAbility Nothing $ ActionCost 1)
        & (abilityLimitL .~ PlayerLimit PerGame 1)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (TakeResources iid 3 False)
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
